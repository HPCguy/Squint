/*
 * mc is capable of compiling a (subset of) C source files into GNU/Linux
 * executables or running via just-in-time compilation on 32-bit ARM
 * processor-based platforms. There is no preprocessor.
 *
 * The following options are supported:
 *   -s : Print source and generated intermediate representation (IR).
 *   -o : Create executable file and terminate normally.
 *
 * If -o and -s are omitted, the compiled code is executed immediately (if
 * there were no compile errors) with the command line arguments passed
 * after the source file parameter.
 *
 * All modifications as of Feb 19 2022 are by HPCguy.
 * See AMaCC project repository for baseline code prior to that date.
 */

#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <memory.h>
#include <sys/mman.h>
#include <unistd.h>
#include <sys/types.h>
#include <sys/stat.h>
#include <fcntl.h>
#include <dlfcn.h>

#if 0
float strtof(char *s, char **e);
#endif

#ifdef SQUINT_SO
void squint_opt(int *begin, int *end);
#endif

char *freep, *p, *lp; // current position in source code
char *freedata, *data, *_data;   // data/bss pointer

int *e, *le, *text; // current position in emitted code
int *cas;           // case statement patch-up pointer
int *def;           // default statement patch-up pointer
int *brks;          // break statement patch-up pointer
int *cnts;          // continue statement patch-up pointer
int  swtc;          // !0 -> in a switch-stmt context
int  brkc;          // !0 -> in a break-stmt context
int  cntc;          // !0 -> in a continue-stmt context
int *tsize;         // array (indexed by type) of type sizes
int tnew;           // next available type
int tk;             // current token
union conv {
   int i;
   float f;
} tkv;              // current token value
int ty;             // current expression type
int loc;            // local variable offset
int line;           // current line number
int src;            // print source and assembly flag
int signed_char;    // use `signed char` for `char`
int elf;            // print ELF format
int peephole;       // helper for peephole optimization
int *n;             // current position in emitted abstract syntax tree
                    // With an AST, the compiler is not limited to generate
                    // code on the fly with parsing.
                    // This capability allows function parameter code to be
                    // emitted and pushed on the stack in the proper
                    // right-to-left order.
int ld;             // local variable depth
int pplev, pplevt;  // preprocessor conditional level

// identifier
struct ident_s {
   int tk;         // type-id or keyword
   int hash;
   char *name;     // name of this identifier
   /* fields starting with 'h' were designed to save and restore
    * the global class/type/val in order to handle the case if a
    * function declares a local with the same name as a global.
    */
   int class, hclass; // FUNC, GLO (global var), LOC (local var), Syscall
   int type, htype;   // data type such as char and int
   int val, hval;
   int etype; // extended type info
} *id,  // currently parsed identifier
  *sym; // symbol table (simple list of identifiers)

// (library) external functions
struct ef_s {
   char *name;
   int addr;
} **ef_cache;
int ef_count;

struct member_s {
   struct ident_s *id;
   int offset;
   int type;
   struct member_s *next;
} **members; // array (indexed by type) of struct member lists

// tokens and classes (operators last and in precedence order)
// ( >= 128 so not to collide with ASCII-valued tokens)
enum {
   Func=128, Syscall, Main, ClearCache, Sqrt, Glo, Par, Loc, Keyword, Id, Load,
   Enter, Num, NumF, Enum, Char, Int, Float, Struct, Union, Sizeof, Return, Goto,
   Break, Continue, If, DoWhile, While, For, Switch, Case, Default, Else, Label,
   Assign, // operator =, keep Assign as highest priority operator
   OrAssign, XorAssign, AndAssign, ShlAssign, ShrAssign, // |=, ^=, &=, <<=, >>=
   AddAssign, SubAssign, MulAssign, DivAssign, ModAssign, // +=, -=, *=, /=, %=
   Cond, // operator: ?
   Lor, Lan, Or, Xor, And,         // operator: ||, &&, |, ^, &
   Eq, Ne, Ge, Lt, Gt, Le,         // operator: ==, !=, >=, <, >, <=
   Shl, Shr, Add, Sub, Mul, Div, Mod, // operator: <<, >>, +, -, *, /, %
   AddF, SubF, MulF, DivF,       // float type operators (hidden)
   EqF, NeF, GeF, LtF, GtF, LeF,
   CastF, Inc, Dec, Dot, Arrow, Bracket // operator: ++, --, ., ->, [
};

// opcodes
/* The instruction set is designed for building intermediate representation.
 * Expression 10 + 20 will be translated into the following instructions:
 *    i = 0;
 *    text[i++] = IMM;
 *    text[i++] = 10;
 *    text[i++] = PSH;
 *    text[i++] = IMM;
 *    text[i++] = 20;
 *    text[i++] = ADD;
 *    text[i++] = PSH;
 *    text[i++] = EXIT;
 *    pc = text;
 */
enum {
   LEA , /*  0 */
   /* LEA addressed the problem how to fetch arguments inside sub-function.
    * Let's check out what a calling frame looks like before learning how
    * to fetch arguments (Note that arguments are pushed in its calling
    * order):
    *
    *     sub_function(arg1, arg2, arg3);
    *
    *     |    ....       | high address
    *     +---------------+
    *     | arg: 1        |    new_bp + 4
    *     +---------------+
    *     | arg: 2        |    new_bp + 3
    *     +---------------+
    *     | arg: 3        |    new_bp + 2
    *     +---------------+
    *     |return address |    new_bp + 1
    *     +---------------+
    *     | old BP        | <- new BP
    *     +---------------+
    *     | local var 1   |    new_bp - 1
    *     +---------------+
    *     | local var 2   |    new_bp - 2
    *     +---------------+
    *     |    ....       |  low address
    *
    * If we need to refer to arg1, we need to fetch new_bp + 4, which can not
    * be achieved by restricted ADD instruction. Thus another special
    * instrcution is introduced to do this: LEA <offset>.
    * Together with JSR, ENT, ADJ, LEV, and LEA instruction, we are able to
    * make function calls.
    */

   IMM , /*  1 */
   /* IMM <num> to put immediate <num> into R0 */

   IMMF , /* 2 */
   /* IMM <num> to put immediate <num> into S0 */

   JMP , /*  3 */
   /* JMP <addr> will unconditionally set the value PC register to <addr> */

   JSR , /*  4 */
   /* Jump to address, setting link register for return address */

   BZ  , /*  5 : conditional jump if R0 is zero (jump-if-zero) */
   BNZ , /*  6 : conditional jump if R0 is not zero */

   ENT , /*  7 */
   /* ENT <size> is called when we are about to enter the function call to
    * "make a new calling frame". It will store the current PC value onto
    * the stack, and save some space(<size> bytes) to store the local
    * variables for function.
    */

   ADJ , /*  8 */
   /* ADJ <size> is to adjust the stack, to "remove arguments from frame"
    * The following pseudocode illustrates how ADJ works:
    *     if (op == ADJ) { sp += *pc++; } // add esp, <size>
    */

   LEV , /*  9 */
   /* LEV fetches bookkeeping info to resume previous execution.
    * There is no POP instruction in our design, and the following pseudocode
    * illustrates how LEV works:
    *     if (op == LEV) { sp = bp; bp = (int *) *sp++;
    *                  pc = (int *) *sp++; } // restore call frame and PC
    */

   PSH , /* 10 */
   /* PSH pushes the value in R0 onto the stack */

   PSHF , /* 11 */
   /* PSH pushes the value in R0 onto the stack */

   LC  , /* 12 */
   /* LC loads a character into R0 from a given memory
    * address which is stored in R0 before execution.
    */

   LI  , /* 13 */
   /* LI loads an integer into R0 from a given memory
    * address which is stored in R0 before execution.
    */

   LF  , /* 14 */
   /* LI loads a float into S0 from a given memory
    * address which is stored in R0 before execution.
    */

   SC  , /* 15 */
   /* SC stores the character in R0 into the memory whose
    * address is stored on the top of the stack.
    */

   SI  , /* 16 */
   /* SI stores the integer in R0 into the memory whose
    * address is stored on the top of the stack.
    */

   SF  , /* 17 */
   /* SI stores the float in S0 into the memory whose
    * address is stored on the top of the stack.
    */

   OR  , /* 18 */  XOR , /* 19 */  AND , /* 20 */
   EQ  , /* 21 */  NE  , /* 22 */
   GE  , /* 23 */  LT  , /* 24 */  GT  , /* 25 */ LE  , /* 26 */
   SHL , /* 27 */  SHR , /* 28 */
   ADD , /* 29 */  SUB , /* 30 */  MUL , /* 31 */ DIV , /* 32 */ MOD, /* 33 */
   ADDF, /* 34 */  SUBF, /* 35 */  MULF, /* 36 */ DIVF, /* 37 */
   FTOI, /* 38 */  ITOF, /* 39 */  EQF , /* 40 */ NEF , /* 41 */
   GEF , /* 42 */  LTF , /* 43 */  GTF , /* 44 */ LEF , /* 45 */
   /* arithmetic instructions
    * Each operator has two arguments: the first one is stored on the top
    * of the stack while the second is stored in R0.
    * After the calculation is done, the argument on the stack will be poped
    * off and the result will be stored in R0.
    */

   SQRT, /* 46 float sqrtf(float); */
   SYSC, /* 47 system call */
   CLCA, /* 48 clear cache, used by JIT compilation */

   PHD ,  /* 49 PeepHole Disable next assembly instruction in optimizer */
   PHR0 , /* 50 Inform PeepHole optimizer that R0 holds a return value */

   INVALID
};

// types
enum { CHAR, INT, FLOAT, PTR = 256, PTR2 = 512 };

// ELF generation
char **plt_func_addr;
char *freebuf;

char *append_strtab(char **strtab, char *str)
{
   char *s;
   for (s = str; *s && (*s != ' '); ++s) ; /* ignore trailing space */
   int nbytes = s - str + 1;
   char *res = *strtab;
   memcpy(res, str, nbytes);
   res[s - str] = 0; // null terminator
   *strtab = res + nbytes;
   return res;
}

char fatal(char *msg) { printf("%d: %s\n", line, msg); exit(-1); }

void ef_add(char *name, int addr) // add external function
{
   ef_cache[ef_count] = malloc(sizeof(struct ef_s)) ;
   ef_cache[ef_count]->name = malloc(strlen(name)+1);
   strcpy(ef_cache[ef_count]->name, name);
   ef_cache[ef_count]->addr = addr;
   ++ef_count;
}

int ef_getaddr(int idx) // get address external function
{
   return (elf ? (int) plt_func_addr[idx] : ef_cache[idx]->addr);
}

int ef_getidx(char *name) // get cache index of external function
{
   int i;
   for (i = 0; i < ef_count; ++i)
      if (!strcmp(ef_cache[i]->name, name))
         break;

   if (i == ef_count) { // add new external lib func to cache
      int dladdr;
      if ((dladdr = (int) dlsym(0, name))) {
         ef_add(name, dladdr);
      } else {
         void *divmod_handle = dlopen("libgcc_s.so.1", 1);
         if (!divmod_handle) fatal("failed to open libgcc_s.so.1");
         dladdr = (int) dlsym(divmod_handle, name);
         if (!dladdr) // fatal("bad function call");
         {
            void *libm_handle = dlopen("libm.so.6", 1);
            if (!libm_handle) fatal("failed to open libm.so.6");
            dladdr = (int) dlsym(libm_handle, name);
            if (!dladdr) fatal("bad function call");
         }
         ef_add(name, dladdr);
      }
   }
   return i;
}

/* parse next token
 * 1. store data into id and then set the id to current lexcial form
 * 2. set tk to appropriate type
 */
void next()
{
   char *pp;
   int t;

   /* using loop to ignore whitespace characters, but characters that
    * cannot be recognized by the lexical analyzer are considered blank
    * characters, such as '@' and '$'.
    */
   while ((tk = *p)) {
      ++p;
      if ((tk >= 'a' && tk <= 'z') || (tk >= 'A' && tk <= 'Z') || (tk == '_')) {
         pp = p - 1;
         while ((*p >= 'a' && *p <= 'z') || (*p >= 'A' && *p <= 'Z') ||
                (*p >= '0' && *p <= '9') || (*p == '_'))
            tk = tk * 147 + *p++;
         tk = (tk << 6) + (p - pp);  // hash plus symbol length
         // hash value is used for fast comparison. Since it is inaccurate,
         // we have to validate the memory content as well.
         for (id = sym; id->tk; ++id) { // find one free slot in table
            if (tk == id->hash && // if token is found (hash match), overwrite
               !memcmp(id->name, pp, p - pp)) {
               tk = id->tk;
               return;
            }
         }
         /* At this point, existing symbol name is not found.
          * "id" points to the first unused symbol table entry.
          */
         id->name = pp;
         id->hash = tk;
         tk = id->tk = Id;  // token type identifier
         return;
      }
      /* Calculate the constant */
      // first byte is a number, and it is considered a numerical value
      else if (tk >= '0' && tk <= '9') {
         tk = Num; // token is char or int
         tkv.i = strtoul((pp = p - 1), &p, 0); // octal, decimal, hex parsing
         if (*p == '.') { tkv.f = strtof(pp, &p); tk = NumF; } // float
         return;
      }
      switch (tk) {
      case '\n':
         if (src) {
            int *base = le;
            printf("%d: %.*s", line, p - lp, lp);
            lp = p;
            while (le < e) {
               int off = le - base; // Func IR instruction memory offset
               printf("%04d: %8.4s", off,
                     & "LEA  IMM  IMMF JMP  JSR  BZ   BNZ  ENT  ADJ  LEV  "
                       "PSH  PSHF LC   LI   LF   SC   SI   SF   "
                       "OR   XOR  AND  EQ   NE   GE   LT   GT   LE   "
                       "SHL  SHR  ADD  SUB  MUL  DIV  MOD  "
                       "ADDF SUBF MULF DIVF FTOI ITOF "
                       "EQF  NEF  GEF  LTF  GTF  LEF  SQRT "
                       "SYSC CLCA PHD  PHR0" [*++le * 5]);
               if (*le < ADJ) {
                  ++le;
                  if (*le > (int) base && *le <= (int) e)
                     printf(" %04d\n", off + ((*le - (int) le) >> 2) + 1);
                  else if (*le < -256000000) {
                     struct ident_s *scan = sym;
                     for (; scan->tk; ++scan)
                        if (scan->val == *le) {
                           printf(" &%.*s", scan->hash & 0x3f, scan->name);
                           if (src == 2) printf(" (%08x)", *le);
                           printf("\n");
                           break;
                        }
                     if (!scan->tk) printf(" %08x\n", *le);
                  }
                  else
                     printf(" %d\n", *le);
               }
               else if (*le == ADJ) {
                  ++le;
                  printf(" %d\n", *le & 0xf);
               }
               else if (*le == SYSC) {
                  printf(" %s\n", ef_cache[*(++le)]->name);
               }
               else printf("\n");
            }
         }
         ++line;
      case ' ':
      case '\t':
      case '\v':
      case '\f':
      case '\r':
         break;
      case '/':
         if (*p == '/') { // comment
            while (*p != 0 && *p != '\n') ++p;
         } else if (*p == '*') { // C-style multiline comments
            t = 0;
            for (++p; (*p != 0) && (t == 0); ++p) {
               pp = p + 1;
               if (*p == '\n') ++line;
               else if (*p == '*' && *pp == '/') t = 1;
            }
            ++p;
         } else {
            if (*p == '=') { ++p; tk = DivAssign; }
            else tk = Div; return;
         }
         break;
      case '#': // skip include statements, and most preprocessor directives
         if (!strncmp(p, "define", 6)) {
            p += 6; next();
            if (tk == Id) {
               next();
               if (tk == Num) {
                  id->class = Num; id->type = INT; id->val = tkv.i;
               }
            }
         }
         else if ((t = !strncmp(p, "ifdef", 5)) || !strncmp(p, "ifndef", 6)) {
            p += 6; next();
            if (tk != Id) fatal("No identifier");
            ++pplev;
            if (( ((id->class != Num) ? 0 : 1) ^ (t ? 1 : 0) ) & 1) {
               t = pplevt; pplevt = pplev - 1;
               while (*p != 0 && *p != '\n') ++p; // discard until end-of-line
               do next(); while (pplev != pplevt);
               pplevt = t;
            }
         }
         else if (!strncmp(p, "if", 2)) {
            // ignore side effects of preprocessor if-statements
            ++pplev;
         }
         else if(!strncmp(p, "endif", 5)) {
            if (--pplev < 0) fatal("preprocessor context nesting error");
            if (pplev == pplevt) return;
         }
         while (*p != 0 && *p != '\n') ++p; // discard until end-of-line
         break;
      case '\'': // quotes start with character (string)
      case '"':
         pp = data;
         while (*p != 0 && *p != tk) {
            if ((tkv.i = *p++) == '\\') {
               switch (tkv.i = *p++) {
               case 'n': tkv.i = '\n'; break; // new line
               case 't': tkv.i = '\t'; break; // horizontal tab
               case 'v': tkv.i = '\v'; break; // vertical tab
               case 'f': tkv.i = '\f'; break; // form feed
               case 'r': tkv.i = '\r'; break; // carriage return
               case '0': tkv.i = '\0'; break; // an int with value 0
               }
            }
            // if it is double quotes (string literal), it is considered as
            // a string, copying characters to data
            if (tk == '"') *data++ = tkv.i;
         }
         ++p;
         if (tk == '"') tkv.i = (int) pp; else tk = Num;
         return;
      case '=': if (*p == '=') { ++p; tk = Eq; } else tk = Assign; return;
      case '*': if (*p == '=') { ++p; tk = MulAssign; }
                else tk = Mul; return;
      case '+': if (*p == '+') { ++p; tk = Inc; }
                else if (*p == '=') { ++p; tk = AddAssign; }
                else tk = Add; return;
      case '-': if (*p == '-') { ++p; tk = Dec; }
                else if (*p == '>') { ++p; tk = Arrow; }
                else if (*p == '=') { ++p; tk = SubAssign; }
                else tk = Sub; return;
      case '[': tk = Bracket; return;
      case '&': if (*p == '&') { ++p; tk = Lan; }
                else if (*p == '=') { ++p; tk = AndAssign; }
                else tk = And; return;
      case '!': if (*p == '=') { ++p; tk = Ne; } return;
      case '<': if (*p == '=') { ++p; tk = Le; }
                else if (*p == '<') {
                   ++p; if (*p == '=') { ++p ; tk = ShlAssign; } else tk = Shl;
                }
                else tk = Lt; return;
      case '>': if (*p == '=') { ++p; tk = Ge; }
                else if (*p == '>') {
                   ++p; if (*p == '=') { ++p ; tk = ShrAssign; } else tk = Shr;
                }
                else tk = Gt; return;
      case '|': if (*p == '|') { ++p; tk = Lor; }
                else if (*p == '=') { ++p; tk = OrAssign; }
                else tk = Or; return;
      case '^': if (*p == '=') { ++p; tk = XorAssign; } else tk = Xor; return;
      case '%': if (*p == '=') { ++p; tk = ModAssign; }
                else tk = Mod; return;
      case '?': tk = Cond; return;
      case '.': tk = Dot; return;
      default: return;
      }
   }
}

int popcount(int i)
{
   i = i - ((i >> 1) & 0x55555555); // add pairs of bits
   i = (i & 0x33333333) + ((i >> 2) & 0x33333333); // quads
   i = (i + (i >> 4)) & 0x0F0F0F0F; // groups of 8
   return (i * 0x01010101) >> 24; // horizontal sum of bytes
}

/* expression parsing
 * lev represents an operator.
 * because each operator `token` is arranged in order of priority,
 * large `lev` indicates a high priority.
 *
 * Operator precedence (lower first):
 * Assign  =
 * Cond   ?
 * Lor    ||
 * Lan    &&
 * Or     |
 * Xor    ^
 * And    &
 * Eq     ==
 * Ne     !=
 * Ge     >=
 * Lt     <
 * Gt     >
 * Le     <=
 * Shl    <<
 * Shr    >>
 * Add    +
 * Sub    -
 * Mul    *
 * Div    /
 * Mod    %
 * Inc    ++
 * Dec    --
 * Bracket [
 */
void expr(int lev)
{
   int otk;
   int t, tc, tt, nf, *b, sz, *c;
   union conv *c1, *c2;
   struct ident_s *d;
   struct member_s *m /* jk = 0 */;

   switch (tk) {
   case Id:
      d = id; next();
      // function call
      if (tk == '(') {
         if (d->class == Func && d->val == 0) goto resolve_fnproto;
         if (d->class < Func || d->class > Sqrt) {
            if (d->class != 0) fatal("bad function call");
            d->type = INT;
            d->etype = 0;
            // printf("%d: %.*s(): assuming any/all args are type int\n",
            //        line, d->hash & 0x3f, d->name);
resolve_fnproto:
            d->class = Syscall;
            int namelen = d->hash & 0x3f;
            char ch = d->name[namelen];
            d->name[namelen] = 0;
            d->val = ef_getidx(d->name) ;
            d->name[namelen] = ch;
         }
         next();
         t = 0; b = 0; tt = 0; nf = 0; // argument count
         while (tk != ')') {
            expr(Assign); *--n = (int) b; b = n; ++t;
            tt = tt * 2; if (ty == FLOAT) { ++nf; ++tt; }
            if (tk == ',') {
               next();
               if (tk == ')') fatal("unexpected comma in function call");
            } else if (tk != ')') fatal("missing comma in function call");
         }
         tt = (tt << 8) + (nf << 4) + t;
         if (d->etype && (d->etype != tt) ) fatal("argument type mismatch");
         next();
         // function or system call id
         *--n = tt; *--n = t; *--n = d->val; *--n = (int) b; *--n = d->class;
         ty = d->type;
      }
      // enumeration, only enums have ->class == Num
      else if (d->class == Num) { *--n = d->val; *--n = Num; ty = INT; }
      else {
         // Variable get offset
         switch (d->class) {
         case Loc: case Par: *--n = loc - d->val; *--n = Loc; break;
         case Glo: *--n = d->val; *--n = Num; break;
         default: fatal("undefined variable");
         }
         *--n = ty = d->type; *--n = Load;
      }
      break;
   // directly take an immediate value as the expression value
   // IMM recorded in emit sequence
   case Num : *--n = tkv.i; *--n = Num;  next(); ty = INT; break;
   case NumF: *--n = tkv.i; *--n = NumF; next(); ty = FLOAT; break;
   case '"': // string, as a literal in data segment
      *--n = tkv.i; *--n = Num; next();
      // continuous `"` handles C-style multiline text such as `"abc" "def"`
      while (tk == '"') next();
      data = (char *) (((int) data + sizeof(int)) & (-sizeof(int)));
      ty = CHAR + PTR;
      break;
   /* SIZEOF_expr -> 'sizeof' '(' 'TYPE' ')'
    * FIXME: not support "sizeof (Id)".
    */
   case Sizeof:
      next();
      if (tk != '(') fatal("open parentheses expected in sizeof");
      next();
      ty = INT; // Enum
      switch (tk) {
      case Char:
      case Int:
      case Float:
         ty = tk - Char; next(); break;
      case Struct:
      case Union:
         next();
         if (tk != Id) fatal("bad struct/union type");
         ty = id->etype; next(); break;
      }
      // multi-level pointers, plus `PTR` for each level
      while (tk == Mul) { next(); ty += PTR; }
      if (tk != ')') fatal("close parentheses expected in sizeof");
      next();
      *--n = (ty >= PTR) ? sizeof(int) : tsize[ty]; *--n = Num;
      ty = INT;
      break;
   // Type cast or parenthesis
   case '(':
      next();
      if (tk >= Char && tk <= Union) {
         switch (tk) {
         case Char:
         case Int:
         case Float:
            t = tk - Char; next(); break;
         default:
            next();
            if (tk != Id) fatal("bad struct/union type");
            t = id->etype; next(); break;
         }
         // t: pointer
         while (tk == Mul) { next(); t += PTR; }
         if (tk != ')') fatal("bad cast");
         next();
         expr(Inc); // cast has precedence as Inc(++)
         if (((t ^ ty) & FLOAT) && (t == FLOAT || ty == FLOAT)) {
            if (t == FLOAT && ty == INT) {
               if (*n == Num) { *n = NumF; c1 = &n[1]; c1->f = c1->i; }
               else { b = n; *--n = ITOF; *--n = (int) b; *--n = CastF; }
            }
            else if (t == INT && ty == FLOAT) {
               if (*n == NumF) { *n = Num; c1 = &n[1]; c1->i = c1->f; }
               else { b = n; *--n = FTOI; *--n = (int) b; *--n = CastF; }
            }
            else fatal("explicit cast required");
         }
         ty = t;
      } else {
         expr(Assign);
         while (tk == ',') { next(); expr(Assign); }
         if (tk != ')') fatal("close parentheses expected");
         next();
      }
      break;
   case Mul: // "*", dereferencing the pointer operation
      next();
      expr(Inc); // dereference has the same precedence as Inc(++)
      if (ty < PTR) fatal("bad dereference");
      ty -= PTR;
      if (ty < CHAR || ty >= PTR2) fatal("unexpected type");
      *--n = ty; *--n = Load; // jk PTR2 test too strict?
      break;
   case And: // "&", take the address operation
      /* when "token" is a variable, it takes the address first and
       * then LI/LC, so `--e` becomes the address of "a".
       */
      next(); expr(Inc);
      if (*n != Load) fatal("bad address-of");
      n += 2;
      ty += PTR;
      break;
   case '!': // "!x" is equivalent to "x == 0"
      next(); expr(Inc);
      if (*n == Num) n[1] = !n[1];
      else { *--n = 0; *--n = Num; --n; *n = (int) (n + 3); *--n = Eq; }
      ty = INT;
      break;
   case '~': // "~x" is equivalent to "x ^ -1"
      next(); expr(Inc);
      if (*n == Num) n[1] = ~n[1];
      else { *--n = -1; *--n = Num; --n; *n = (int) (n + 3); *--n = Xor; }
      ty = INT;
      break;
   case Add:
      next(); expr(Inc); if (ty != FLOAT) ty = INT;
      break;
   case Sub:
      next();
      expr(Inc);
      if (*n == Num) n[1] = -n[1];
      else if (*n == NumF) { n[1] ^= 0x80000000; }
      else if (ty == FLOAT) {
         *--n = 0xbf800000; *--n = NumF; --n; *n = (int) (n + 3); *--n = MulF;
      }
      else {
         *--n = -1; *--n = Num; --n; *n = (int) (n + 3); *--n = Mul;
      }
      if (ty != FLOAT) ty = INT;
      break;
   case Div:
   case Mod:
      break;
   // processing ++x and --x. x-- and x++ is handled later
   case Inc:
   case Dec:
      t = tk; next(); expr(Inc);
      if (*n != Load) fatal("bad lvalue in pre-increment");
      *n = t;
      break;
   case 0: fatal("unexpected EOF in expression");
   default: fatal("bad expression");
   }

   // "precedence climbing" or "Top Down Operator Precedence" method
   while (tk >= lev) {
      // tk is ASCII code will not exceed `Num=128`. Its value may be changed
      // during recursion, so back up currently processed expression type
      t = ty; b = n;
      switch (tk) {
      case Assign:
         next();
         // the left part is processed by the variable part of `tk=ID`
         // and pushes the address
         if (*n != Load) fatal("bad lvalue in assignment");
         // get the value of the right part `expr` as the result of `a=expr`
         expr(Assign); *--n = (int) (b + 2);
                       *--n = (ty << 16) | t; *--n = Assign; ty = t;
         break;
      case  OrAssign: // right associated
      case XorAssign:
      case AndAssign:
      case ShlAssign:
      case ShrAssign:
      case AddAssign:
      case SubAssign:
      case MulAssign:
      case DivAssign:
      case ModAssign:
         otk = tk;
         *--n=';'; *--n = t; *--n = Load;
         sz = (ty = t) >= PTR2 ? sizeof(int) :
                              ty >= PTR ? tsize[ty - PTR] : 1;
         next(); c = n; expr(otk);
         if (*n == Num) n[1] *= sz;
         *--n = (int) c;
         if (otk < ShlAssign) {
            *--n = Or + (otk - OrAssign);
         } else {
            *--n = Shl + (otk - ShlAssign);
            // Compound-op bypasses literal const optimizations
            if (otk == DivAssign) ef_getidx("__aeabi_idiv");
            if (otk == ModAssign) ef_getidx("__aeabi_idivmod");
         }
         *--n = (int) (b + 2); *--n = ty = t; *--n = Assign;
         ty = INT;
         break;
      case Cond: // `x?a:b` is similar to if except that it relies on else
         next(); expr(Assign); tc = ty;
         if (tk != ':') fatal("conditional missing colon");
         next(); c = n;
         expr(Cond); --n; if (tc != ty) fatal("both results need same type");
         *n = (int) (n + 1); *--n = (int) c; *--n = (int) b; *--n = Cond;
         break;
      case Lor: // short circuit, the logical or
         next(); expr(Lan);
         if (*n == Num && *b == Num) n[1] = b[1] || n[1];
         else { *--n = (int) b; *--n = Lor; }
         ty = INT;
         break;
      case Lan: // short circuit, logic and
         next(); expr(Or);
         if (*n == Num && *b == Num) n[1] = b[1] && n[1];
         else { *--n = (int) b; *--n = Lan; }
         ty = INT;
         break;
      case Or: // push the current value, calculate the right value
         next(); expr(Xor);
         if (*n == Num && *b == Num) n[1] = b[1] | n[1];
         else { *--n = (int) b; *--n = Or; }
         ty = INT;
         break;
      case Xor:
         next(); expr(And);
         if (*n == Num && *b == Num) n[1] = b[1] ^ n[1];
         else { *--n = (int) b; *--n = Xor; }
         ty = INT;
         break;
      case And:
         next(); expr(Eq);
         if (*n == Num && *b == Num) n[1] = b[1] & n[1];
         else { *--n = (int) b; *--n = And; }
         ty = INT;
         break;
      case Eq:
         tc = ty; next(); expr(Ge);
         if (tc != ty && (tc == FLOAT || ty == FLOAT)) fatal("type mismatch");
         if (ty == FLOAT) {
            if (*n == NumF && *b == NumF) {
               c1 = &n[1]; c2 = &b[1]; c1->i = (c2->f == c1->f);
               *n = Num; ty = INT;
            }
            else { *--n = (int) b; *--n = EqF; }
         } else {
            if (*n == Num && *b == Num) n[1] = b[1] == n[1];
            else { *--n = (int) b; *--n = Eq; }
            ty = INT;
         }
         break;
      case Ne:
         tc = ty; next(); expr(Ge);
         if (tc != ty && (tc == FLOAT || ty == FLOAT)) fatal("type mismatch");
         if (ty == FLOAT) {
            if (*n == NumF && *b == NumF) {
               c1 = &n[1]; c2 = &b[1]; c1->i = (c2->f != c1->f);
               *n = Num; ty = INT;
            }
            else { *--n = (int) b; *--n = NeF; }
         } else {
            if (*n == Num && *b == Num) n[1] = b[1] != n[1];
            else { *--n = (int) b; *--n = Ne; }
            ty = INT;
         }
         break;
      case Ge:
         tc = ty; next(); expr(Shl);
         if (tc != ty && (tc == FLOAT || ty == FLOAT)) fatal("type mismatch");
         if (ty == FLOAT) {
            if (*n == NumF && *b == NumF) {
               c1 = &n[1]; c2 = &b[1]; c1->i = (c2->f >= c1->f);
               *n = Num; ty = INT;
            }
            else { *--n = (int) b; *--n = GeF; }
         } else {
            if (*n == Num && *b == Num) n[1] = b[1] >= n[1];
            else { *--n = (int) b; *--n = Ge; }
            ty = INT;
         }
         break;
      case Lt:
         tc = ty; next(); expr(Shl);
         if (tc != ty && (tc == FLOAT || ty == FLOAT)) fatal("type mismatch");
         if (ty == FLOAT) {
            if (*n == NumF && *b == NumF) {
               c1 = &n[1]; c2 = &b[1]; c1->i = (c2->f < c1->f);
               *n = Num; ty = INT;
            }
            else { *--n = (int) b; *--n = LtF; }
         } else {
            if (*n == Num && *b == Num) n[1] = b[1] < n[1];
            else { *--n = (int) b; *--n = Lt; }
            ty = INT;
         }
         break;
      case Gt:
         tc = ty; next(); expr(Shl);
         if (tc != ty && (tc == FLOAT || ty == FLOAT)) fatal("type mismatch");
         if (ty == FLOAT) {
            if (*n == NumF && *b == NumF) {
               c1 = &n[1]; c2 = &b[1]; c1->i = (c2->f > c1->f);
               *n = Num; ty = INT;
            }
            else { *--n = (int) b; *--n = GtF; }
         } else {
            if (*n == Num && *b == Num) n[1] = b[1] > n[1];
            else { *--n = (int) b; *--n = Gt; }
            ty = INT;
         }
         break;
      case Le:
         tc = ty; next(); expr(Shl);
         if (tc != ty && (tc == FLOAT || ty == FLOAT)) fatal("type mismatch");
         if (ty == FLOAT) {
            if (*n == NumF && *b == NumF) {
               c1 = &n[1]; c2 = &b[1]; c1->i = (c2->f <= c1->f);
               *n = Num; ty = INT;
            }
            else { *--n = (int) b; *--n = LeF; }
         } else {
            if (*n == Num && *b == Num) n[1] = b[1] <= n[1];
            else { *--n = (int) b; *--n = Le; }
            ty = INT;
         }
         break;
      case Shl:
         next(); expr(Add);
         if (*n == Num && *b == Num) {
            if (n[1] < 0) n[1] = b[1] >> -n[1];
            else n[1] = b[1] << n[1];
         } else { *--n = (int) b; *--n = Shl; }
         ty = INT;
         break;
      case Shr:
         next(); expr(Add);
         if (*n == Num && *b == Num) {
            if (n[1] < 0) n[1] = b[1] << -n[1];
            else n[1] = b[1] >> n[1];
         } else { *--n = (int) b; *--n = Shr; }
         ty = INT;
         break;
      case Add:
         tc = ty; next(); expr(Mul);
         if (tc != ty && (tc == FLOAT || ty == FLOAT)) fatal("type mismatch");
         if (ty == FLOAT) {
            if (*n == NumF && *b == NumF) { 
               c1 = &n[1]; c2 = &b[1]; c1->f = c1->f + c2->f;
            }
            else { *--n = (int) b; *--n = AddF; }
         }
         else {
            sz = (ty = t) >= PTR2 ? sizeof(int) :
                                 ty >= PTR ? tsize[ty - PTR] : 1;
            if (*n == Num) n[1] *= sz;
            if (*n == Num && *b == Num) n[1] += b[1];
            else { *--n = (int) b; *--n = Add; }
         }
         break;
      case Sub:
         tc = ty; next(); expr(Mul);
         if (tc != ty && (tc == FLOAT || ty == FLOAT)) fatal("type mismatch");
         if (ty == FLOAT) {
            if (*n == NumF && *b == NumF) { 
               c1 = &n[1]; c2 = &b[1]; c1->f = c2->f - c1->f;
            }
            else { *--n = (int) b; *--n = SubF; }
         }
         else {
            sz = t >= PTR2 ? sizeof(int) : t >= PTR ? tsize[t - PTR] : 1;
            if (sz > 1 && *n == Num) {
               *--n = sz; *--n = Num; --n; *n = (int) (n + 3); *--n = Mul;
            }
            if (*n == Num && *b == Num) n[1] = b[1] - n[1];
            else {
               *--n = (int) b; *--n = Sub;
               if (t == ty && sz > 1) {
                  switch (sz) {
                  case 4: *--n = 2; *--n = Num; --n; *n = (int) (n + 3);
                         *--n = Shr; break;
                  default: *--n = sz; *--n = Num; --n; *n = (int) (n + 3);
                          *--n = Sub;
                  }
               }
            }
         }
         break;
      case Mul:
         tc = ty; next(); expr(Inc);
         if (tc != ty && (tc == FLOAT || ty == FLOAT)) fatal("type mismatch");
         if (ty == FLOAT) {
            if (*n == NumF && *b == NumF) {
               c1 = &n[1]; c2 = &b[1] ; c1->f = c1->f * c2->f;
            }
            else { *--n = (int) b; *--n = MulF; }
         }
         else {
            if (*n == Num && *b == Num) n[1] *= b[1];
            else {
               *--n = (int) b;
               if (n[1] == Num && n[2] > 0 && (n[2] & (n[2] - 1)) == 0) {
                  n[2] = popcount(n[2] - 1); *--n = Shl; // 2^n
               }
               else *--n = Mul;
            }
            ty = INT;
         }
         break;
      case Inc:
      case Dec:
         sz = ty >= PTR2 ? sizeof(int) : ty >= PTR ? tsize[ty - PTR] : 1;
         if (*n != Load) fatal("bad lvalue in post-increment");
         *n = tk;
         *--n = sz; *--n = Num;
         *--n = (int) b; *--n = (tk == Inc) ? Sub : Add;
         next();
         break;
      case Div:
         tc = ty; next(); expr(Inc);
         if (tc != ty && (tc == FLOAT || ty == FLOAT)) fatal("type mismatch");
         if (ty == FLOAT) {
            if (*n == NumF && *b == NumF) {
               c1 = &n[1]; c2 = &b[1] ; c1->f = c2->f / c1->f;
            }
            else { *--n = (int) b; *--n = DivF; }
         }
         else {
            if (*n == Num && *b == Num) n[1] = b[1] / n[1];
            else {
               *--n = (int) b;
               if (n[1] == Num && n[2] > 0 && (n[2] & (n[2] - 1)) == 0) {
                  n[2] = popcount(n[2] - 1); *--n = Shr; // 2^n
               } else {
                  *--n = Div;
                  ef_getidx("__aeabi_idiv");
               }
            }
            ty = INT;
         }
         break;
      case Mod:
         next(); expr(Inc);
         if (*n == Num && *b == Num) n[1] = b[1] % n[1];
         else {
            *--n = (int) b;
            if (n[1] == Num && n[2] > 0 && (n[2] & (n[2] - 1)) == 0) {
               --n[2]; *--n = And; // 2^n
            } else {
               *--n = Mod;
               ef_getidx("__aeabi_idivmod");
            }
         }
         ty = INT;
         break;
      case Dot:
         ty += PTR;
         if (n[0] == Load && n[1] > FLOAT && n[1] < PTR) n += 2; // struct
      case Arrow:
         if (ty <= PTR+FLOAT || ty >= PTR2) fatal("structure expected");
         next();
         if (tk != Id) fatal("structure member expected");
         m = members[ty - PTR]; while (m && m->id != id) m = m->next;
         if (!m) fatal("structure member not found");
         if (m->offset) {
            *--n = m->offset; *--n = Num; --n; *n = (int) (n + 3);
            *--n = Add;
         }
         ty = m->type;
         *--n = (ty >= PTR) ? INT : ty; *--n = Load;
         next();
         break;
      case Bracket:
         next(); expr(Assign);
         if (tk != ']') fatal("close bracket expected");
         next();
         if (t < PTR) fatal("pointer type expected");
         sz = (t = t - PTR) >= PTR ? sizeof(int) : tsize[t];
         if (sz > 1) {
            if (*n == Num) n[1] *= sz;
            else {
               *--n = sz; *--n = Num; --n; *n = (int) (n + 3); *--n = Mul;
            }
         }
         if (*n == Num && *b == Num) n[1] += b[1];
         else { *--n = (int) b; *--n = Add; }
         *--n = ((ty = t) >= PTR) ? INT : ty; *--n = Load;
         break;
      default:
         printf("%d: compiler error tk=%d\n", line, tk); exit(-1);
      }
   }
}

// AST parsing for IR generatiion
// With a modular code generator, new targets can be easily supported such as
// native Arm machine code.
void gen(int *n)
{
   int i = *n, j, k, l;
   int *a, *b, *c, *d, *t;
   struct ident_s *label;

   switch (i) {
   case Num: *++e = IMM; *++e = n[1]; break;   // int value
   case NumF: *++e = IMMF; *++e = n[1]; break; // float value
   case Load:
      gen(n + 2); // load the value
      if (n[1] > FLOAT && n[1] < PTR) fatal("struct copies not yet supported");
      *++e = (n[1] >= PTR) ? LI : LC + n[1];
      break;
   case Loc: *++e = LEA; *++e = n[1]; break;       // get address of variable
   case '{': gen((int *) n[1]); gen(n + 2); break; // parse AST expr or stmt
   case Assign: // assign the value to variables
      gen((int *) n[2]); *++e = PSH; gen(n + 3); l = n[1] & 0xffff;
      // Add SC/SI instruction to save value in register to variable address
      // held on stack.
      if (l > FLOAT && l < PTR) fatal("struct assign not yet supported");
      if ((n[1] >> 16) == FLOAT && l == INT) *++e = FTOI;
      else if ((n[1] >> 16) == INT && l == FLOAT) *++e = ITOF;
      *++e = (l >= PTR) ? SI : SC + l;
      break;
   case Inc: // increment or decrement variables
   case Dec:
      gen(n + 2);
      *++e = PSH; *++e = (n[1] == CHAR) ? LC : LI; *++e = PSH;
      *++e = IMM; *++e = (n[1] >= PTR2) ? sizeof(int) :
                                      n[1] >= PTR ? tsize[n[1] - PTR] : 1;
      *++e = (i == Inc) ? ADD : SUB;
      *++e = (n[1] == CHAR) ? SC : SI;
      break;
   case Cond: // if else condition case
      gen((int *) n[1]); // condition
      // Add jump-if-zero instruction "BZ" to jump to false branch.
      // Point "b" to the jump address field to be patched later.
      *++e = BZ; b = ++e;
      gen((int *) n[2]); // expression
      // Patch the jump address field pointed to by "b" to hold the address
      // of false branch. "+ 3" counts the "JMP" instruction added below.
      //
      // Add "JMP" instruction after true branch to jump over false branch.
      // Point "b" to the jump address field to be patched later.
      if (n[3]) {
         *b = (int) (e + 3); *++e = JMP; b = ++e; gen((int *) n[3]);
      } // else statment
      // Patch the jump address field pointed to by "d" to hold the address
      // past the false branch.
      *b = (int) (e + 1);
      break;
   // operators
   /* If current token is logical OR operator:
    * Add jump-if-nonzero instruction "BNZ" to implement short circuit.
    * Point "b" to the jump address field to be patched later.
    * Parse RHS expression.
    * Patch the jump address field pointed to by "b" to hold the address past
    * the RHS expression.
    */
   case Lor:  gen((int *) n[1]); *++e = BNZ;
              b = ++e; gen(n + 2); *b = (int) (e + 1); break;
   case Lan:  gen((int *) n[1]); *++e = BZ;
              b = ++e; gen(n + 2); *b = (int) (e + 1); break;
   /* If current token is bitwise OR operator:
    * Add "PSH" instruction to push LHS value in register to stack.
    * Parse RHS expression.
    * Add "OR" instruction to compute the result.
    */
   case Or:   gen((int *) n[1]); *++e = PSH; gen(n + 2); *++e = OR; break;
   case Xor:  gen((int *) n[1]); *++e = PSH; gen(n + 2); *++e = XOR; break;
   case And:  gen((int *) n[1]); *++e = PSH; gen(n + 2); *++e = AND; break;
   case Eq:   gen((int *) n[1]); *++e = PSH; gen(n + 2); *++e = EQ; break;
   case Ne:   gen((int *) n[1]); *++e = PSH; gen(n + 2); *++e = NE; break;
   case Ge:   gen((int *) n[1]); *++e = PSH; gen(n + 2); *++e = GE; break;
   case Lt:   gen((int *) n[1]); *++e = PSH; gen(n + 2); *++e = LT; break;
   case Gt:   gen((int *) n[1]); *++e = PSH; gen(n + 2); *++e = GT; break;
   case Le:   gen((int *) n[1]); *++e = PSH; gen(n + 2); *++e = LE; break;
   case Shl:  gen((int *) n[1]); *++e = PSH; gen(n + 2); *++e = SHL; break;
   case Shr:  gen((int *) n[1]); *++e = PSH; gen(n + 2); *++e = SHR; break;
   case Add:  gen((int *) n[1]); *++e = PSH; gen(n + 2); *++e = ADD; break;
   case Sub:  gen((int *) n[1]); *++e = PSH; gen(n + 2); *++e = SUB; break;
   case Mul:  gen((int *) n[1]); *++e = PSH; gen(n + 2); *++e = MUL; break;
   case Div:  gen((int *) n[1]);
              if (peephole) *++e = PHD;
              *++e = PSH; gen(n + 2); *++e = DIV;
              if (peephole) *++e = PHR0;
              break;
   case Mod:  gen((int *) n[1]);
              if (peephole) *++e = PHD;
              *++e = PSH; gen(n + 2); *++e = MOD;
              if (peephole) *++e = PHR0;
              break;
   case AddF: gen((int *) n[1]); *++e = PSHF; gen(n + 2); *++e = ADDF; break;
   case SubF: gen((int *) n[1]); *++e = PSHF; gen(n + 2); *++e = SUBF; break;
   case MulF: gen((int *) n[1]); *++e = PSHF; gen(n + 2); *++e = MULF; break;
   case DivF: gen((int *) n[1]); *++e = PSHF; gen(n + 2); *++e = DIVF; break;
   case EqF:  gen((int *) n[1]); *++e = PSHF; gen(n + 2); *++e = EQF; break;
   case NeF:  gen((int *) n[1]); *++e = PSHF; gen(n + 2); *++e = NEF; break;
   case GeF:  gen((int *) n[1]); *++e = PSHF; gen(n + 2); *++e = GEF; break;
   case LtF:  gen((int *) n[1]); *++e = PSHF; gen(n + 2); *++e = LTF; break;
   case GtF:  gen((int *) n[1]); *++e = PSHF; gen(n + 2); *++e = GTF; break;
   case LeF:  gen((int *) n[1]); *++e = PSHF; gen(n + 2); *++e = LEF; break;
   case CastF: gen((int *) n[1]); *++e = n[2]; break;
   case Func:
   case Syscall:
   case ClearCache:
      b = (int *) n[1]; k = b ? n[3] : 0;
      if (k) {
         l = (i != ClearCache) ? (n[4] >> 8) : 0;
         a = malloc(sizeof(int *) * k);
         for (j = 0; *b ; b = (int *) *b) a[j++] = (int) b;
         a[j] = (int) b;
         while (j >= 0) { // push arguments
            gen(b + 1);
            if (peephole && i != ClearCache) *++e = PHD;
            *++e = (l & (1 << j)) ? PSHF : PSH; --j; b = (int *) a[j];
         }
         free(a);
      }
      if (i == Syscall) *++e = SYSC;
      if (i == Func) *++e = JSR;
      *++e = n[2];
      if (n[3]) { *++e = ADJ; *++e = (i == Syscall) ? n[4] : n[3]; }
      if (peephole && i != ClearCache) *++e = PHR0;
      break;
   case Sqrt: b = (int *) n[1]; gen(b + 1); *++e = n[2]; break;
   case While:
   case DoWhile:
      if (i == While) { *++e = JMP; a = ++e; }
      d = (e + 1);
      b = brks; brks = 0;
      c = cnts; cnts = 0;
      gen((int *) n[1]); // loop body
      if (i == While) *a = (int) (e + 1);
      while (cnts) { t = (int *) *cnts; *cnts = (int) (e + 1); cnts = t; }
      cnts = c;
      gen((int *) n[2]); // condition
      *++e = BNZ; *++e = (int) d;
      while (brks) { t = (int *) *brks; *brks = (int) (e + 1); brks = t; }
      brks = b;
      break;
   case For:
      gen((int *) n[4]);  // init
      *++e = JMP; a = ++e;
      d = (e + 1);  
      b = brks; brks = 0;
      c = cnts; cnts = 0;
      gen((int *) n[3]); // loop body
      while (cnts) { t = (int *) *cnts; *cnts = (int) (e + 1); cnts = t; }
      cnts = c;
      gen((int *) n[2]); // increment
      *a = (int) (e + 1);
      gen((int *) n[1]); // condition
      *++e = BNZ; *++e = (int) d;
      while (brks) { t = (int *) *brks; *brks = (int) (e + 1); brks = t; }
      brks = b;
      break;
   case Switch:
      gen((int *) n[1]); // condition
      a = cas; *++e = JMP; cas = ++e;
      b = brks; d = def; brks = def = 0;
      gen((int *) n[2]); // case statment
      // deal with no default inside switch case
      *cas = def ? (int) def : (int) (e + 1); cas = a;
      while (brks) { t = (int *) * brks; *brks = (int) (e + 1); brks = t; }
      brks = b; def = d;
      break;
   case Case:
      *++e = JMP; ++e;
      a = 0;
      *e = (int) (e + 7); *++e = PSH; i = *cas; *cas = (int) e;
      gen((int *) n[1]); // condition
      if (e[-1] != IMM) fatal("bad case immediate");
      *++e = SUB; *++e = BNZ; cas = ++e; *e = i + e[-3];
      if (*(int *) n[2] == Switch) a = cas;
      gen((int *) n[2]); // expression
      if (a != 0) cas = a;
      break;
   case Break:    *++e = JMP; *++e = (int) brks; brks = e; break;
   case Continue: *++e = JMP; *++e = (int) cnts; cnts = e; break;
   case Goto:
      label = (struct ident_s *) n[1];
      *++e = JMP; *++e = label->val;
      if (label->class == 0) label->val = (int) e; // Define label address later
      break;
   case Default: def = e + 1; gen((int *) n[1]); break;
   case Return:  if (n[1]) gen((int *) n[1]); *++e = LEV; break;
   case Enter: *++e = ENT; *++e = n[1]; gen(n + 2);
            if (*e != LEV) *++e = LEV; break;
   case Label: // target of goto
      label = (struct ident_s *) n[1];
      if (label->class != 0) fatal("duplicate label definition");
      d = e + 1; b = (int *) label->val;
      while (b != 0) { t = (int *) *b; *b = (int) d; b = t; }
      label->val = (int) d; label->class = Label;
      break;
   default:
      if (i != ';') {
         printf("%d: compiler error gen=%08x\n", line, i); exit(-1);
      }
   }
}

void check_label(int **tt)
{
   if (tk != Id) return;
   char *ss = p;
   while (*ss == ' ' || *ss == '\t') ++ss;
   if (*ss == ':') {
      if (id->class != 0 || !(id->type == 0 || id->type == -1))
         fatal("invalid label");
      id->type = -1 ; // hack for id->class deficiency
      *--n = (int) id; *--n = Label;
      *--n = (int) *tt; *--n = '{'; *tt = n;
      next(); next();
   }
}

// statement parsing (syntax analysis, except for declarations)
void stmt(int ctx)
{
   struct ident_s *dd;
   int *a, *b, *c, *d;
   int i, j, nf, atk;
   int bt;

   if (ctx == Glo && (tk < Enum || tk > Union))
      fatal("syntax: statement used outside function");

   switch (tk) {
   case Enum:
      next();
      // If current token is not "{", it means having enum type name.
      // Skip the enum type name.
      if (tk != '{') next();
      if (tk == '{') {
         next();
         i = 0; // Enum value starts from 0
         while (tk != '}') {
            // Current token should be enum name.
            // If current token is not identifier, stop parsing.
            if (tk != Id) fatal("bad enum identifier");
            dd = id; next();
            if (tk == Assign) {
               next();
               expr(Cond);
               if (*n != Num) fatal("bad enum initializer");
               i = n[1]; n += 2; // Set enum value
            }
            dd->class = Num; dd->type = INT; dd->val = i++;
            if (tk == ',') next(); // If current token is ",", skip.
         }
         next(); // Skip "}"
      } else if (tk == Id) {
         if (ctx != Par) fatal("enum can only be declared as parameter");
         id->type = INT; id->class = ctx; id->val = ld++;
         next();
      }
      return;
   case Char:
   case Int:
   case Float:
   case Struct:
   case Union:
      switch (tk) {
      case Char:
      case Int:
      case Float:
         bt = tk - Char; next(); break;
      case Struct:
      case Union:
         atk = tk; next();
         if (tk == Id) {
            if (!id->etype) id->etype = tnew++;
            bt = id->etype;
            next();
         } else {
            bt = tnew++;
         }
         if (tk == '{') {
            next();
            if (members[bt]) fatal("duplicate structure definition");
            tsize[bt] = 0; // for unions
            i = 0;
            while (tk != '}') {
               int mbt = INT; // Enum
               switch (tk) {
               case Char:
               case Int:
               case Float:
                  mbt = tk - Char; next(); break;
               case Struct:
               case Union:
                  next();
                  if (tk != Id) fatal("bad struct/union declaration");
                  mbt = id->etype;
                  next(); break;
               }
               while (tk != ';') {
                  ty = mbt;
                  // if the beginning of * is a pointer type,
                  // then type plus `PTR` indicates what kind of pointer
                  while (tk == Mul) { next(); ty += PTR; }
                  if (tk != Id) fatal("bad struct member definition");
                  struct member_s *m = malloc(sizeof(struct member_s));
                  m->id = id;
                  m->offset = i;
                  m->type = ty;
                  m->next = members[bt];
                  members[bt] = m;
                  i += (ty >= PTR) ? sizeof(int) : tsize[ty];
                  i = (i + 3) & -4;
                  if (atk == Union) { if (i > tsize[bt]) tsize[bt] = i; i = 0; }
                  next();
                  if (tk == ',') next();
               }
               next();
            }
            next();
            if (atk != Union) tsize[bt] = i;
         }
         break;
      }
      /* parse statement such as 'int a, b, c;'
       * "enum" finishes by "tk == ';'", so the code below will be skipped.
       * While current token is not statement end or block end.
       */
      while (tk != ';' && tk != '}' && tk != ',' && tk != ')') {
         ty = bt;
         // if the beginning of * is a pointer type, then type plus `PTR`
         // indicates what kind of pointer
         while (tk == Mul) { next(); ty += PTR; }
         switch (ctx) { // check non-callable identifiers
         case Glo:
            if (tk != Id) fatal("bad global declaration");
            if (id->class >= ctx) fatal("duplicate global definition");
            break;
         case Loc:
            if (tk != Id) fatal("bad local declaration");
            if (id->class >= ctx) fatal("duplicate local definition");
            break;
         }
         next();
         id->type = ty;
         if (tk == '(') { // function
            if (ctx != Glo) fatal("nested function");
            if (ty > FLOAT && ty < PTR) fatal("return type can't be struct");
            if (id->class == Syscall && id->val)
               fatal("forward decl location failed one pass compilation");
            if (id->class == Func &&
               id->val > (int) text && id->val < (int) e)
               fatal("duplicate global definition");
            dd = id; dd->etype = 0; dd->class = Func; // type is function
            dd->val = (int) (e + 1); // function Pointer? offset/address
            next(); nf = 0; ld = 0; // "ld" is parameter's index.
            while (tk != ')') {
               stmt(Par);
               dd->etype = dd->etype * 2;
               if (ty == FLOAT) { ++nf; ++(dd->etype); }
               if (tk == ',') next();
            }
            next(); dd->etype = (dd->etype << 8) + (nf << 4) + ld; // param info
            if (tk == ';') { dd->val = 0; goto unwind_func; } // fn proto
            if (tk != '{') fatal("bad function definition");
            loc = ++ld;
            next();
            // Not declaration and must not be function, analyze inner block.
            // e represents the address which will store pc
            // (ld - loc) indicates memory size to allocate
            *--n = ';';
            while (tk != '}') {
               int *t = n; check_label(&t); stmt(Loc);
               if (t != n) { *--n = (int) t; *--n = '{'; }
            }
            *--n = ld - loc; *--n = Enter;
            cas = 0;
            gen(n);
unwind_func: id = sym;
            while (id->tk) { // unwind symbol table locals
               if (id->class == Loc || id->class == Par) {
                  id->class = id->hclass;
                  id->type = id->htype;
                  id->val = id->hval;
               }
               else if (id->class == Label) { // clear id for next func
                  id->class = 0; id->val = 0; id->type = 0;
               }
               else if (id->class == 0 && id->type == -1) {
                  printf("%d: label %.*s not defined\n",
                         line, id->hash & 0x3f, id->name);
                  exit(-1);
               }
               id++;
            }
         }
         else {
            int sz = (ty >= PTR) ? sizeof(int) : tsize[ty];
            if (tk == Bracket) { // support 1d global array
               if (ctx != Glo)
                  fatal("Array decl only supported for global variables");
               next(); expr(Cond);
               if (*n != Num) fatal("non-const array size");
               if (tk != ']') fatal("missing ]");
               next();
               sz *= n[1]; n += 2;
               ty += PTR; id->type = ty;
            }
            sz = (sz + 3) & -4;
            id->hclass = id->class; id->class = ctx;
            id->htype = id->type; id->type = ty;
            id->hval = id->val;
            if (ctx == Glo) { id->val = (int) data; data += sz; }
            else if (ctx == Loc) { id->val = (ld += sz / sizeof(int)); }
            else if (ctx == Par) {
               if (ty > FLOAT && ty < PTR) // local struct decl
                  fatal("struct parameters must be pointers");
               id->val = ld++;
            }
            if (ctx == Loc && tk == Assign) {
               int ptk = tk;
               *--n = loc - id->val; *--n = Loc;
               next(); a = n; i = ty; expr(ptk);
               *--n = (int)a; *--n = (ty << 16) | i; *--n = Assign; ty = i;
            }
         }
         if (ctx != Par && tk == ',') next();
      }
      return;
   case If:
      next();
      if (tk != '(') fatal("open parentheses expected");
      next();
      expr(Assign); a = n;
      if (tk != ')') fatal("close parentheses expected");
      next();
      stmt(ctx);
      b = n;
      if (tk == Else) { next(); stmt(ctx); d = n; } else d = 0;
      *--n = (int)d; *--n = (int) b; *--n = (int) a; *--n = Cond;
      return;
   case While:
      next();
      if (tk != '(') fatal("open parentheses expected");
      next();
      expr(Assign); b = n; // condition
      if (tk != ')') fatal("close parentheses expected");
      next();
      ++brkc; ++cntc;
      stmt(ctx); a = n; // parse body of "while"
      --brkc; --cntc;
      *--n = (int) b; *--n = (int) a; *--n = While;
      return;
   case DoWhile:
      next();
      ++brkc; ++cntc;
      stmt(ctx); a = n; // parse body of "do-while"
      --brkc; --cntc;
      if (tk != While) fatal("while expected");
      next();
      if (tk != '(') fatal("open parentheses expected");
      next();
      *--n = ';';
      expr(Assign); b = n;
      if (tk != ')') fatal("close parentheses expected");
      next();
      *--n = (int) b; *--n = (int) a; *--n = DoWhile;
      return;
   case Switch:
      i = 0; j = 0;
      if (cas) j = (int) cas;
      cas = &i;
      next();
      if (tk != '(') fatal("open parentheses expected");
      next();
      expr(Assign);
      a = n;
      if (tk != ')') fatal("close parentheses expected");
      next();
      ++swtc; ++brkc;
      stmt(ctx);
      --swtc; --brkc;
      b = n;
      *--n = (int) b; *--n = (int) a; *--n = Switch;
      if (j) cas = (int *) j;
      return;
   case Case:
      if (!swtc) fatal("case-statement outside of switch");
      i = *cas;
      next();
      expr(Or);
      a = n;
      if (*n != Num) fatal("bad case immediate");
      j = n[1]; n[1] -= i; *cas = j;
      *--n = ';';
      if (tk != ':') fatal("colon expected");
      next();
      stmt(ctx);
      b = n;
      *--n = (int) b;*--n = (int) a; *--n = Case;
      return;
   case Break:
      if (!brkc) fatal("misplaced break statement");
      next();
      if (tk != ';') fatal("semicolon expected");
      next();
      *--n = Break;
      return;
   case Continue:
      if (!cntc) fatal("misplaced continue statement");
      next();
      if (tk != ';') fatal("semicolon expected");
      next();
      *--n = Continue;
      return;
   case Default:
      if (!swtc) fatal("default-statement outside of switch");
      next();
      if (tk != ':') fatal("colon expected");
      next();
      stmt(ctx); a = n;
      *--n = (int) a; *--n = Default;
      return;
   // RETURN_stmt -> 'return' expr ';' | 'return' ';'
   case Return:
      a = 0; next();
      if (tk != ';') { expr(Assign); a = n; }
      *--n = (int) a; *--n = Return;
      if (tk != ';') fatal("semicolon expected");
      next();
      return;
   /* For iteration is implemented as:
    * Init -> Cond -> Bz to end -> Jmp to Body
    * After -> Jmp to Cond -> Body -> Jmp to After
    */
   case For:
      next();
      if (tk != '(') fatal("open parentheses expected");
      next();
      *--n = ';';
      if (tk != ';') expr(Assign);
      while (tk == ',') {
         int *f = n; next(); expr(Assign); *--n = (int) f; *--n = '{';
      }
      d = n;
      if (tk != ';') fatal("semicolon expected");
      next();
      *--n = ';';
      expr(Assign); a = n; // Point to entry of for cond
      if (tk != ';') fatal("semicolon expected");
      next();
      *--n = ';';
      if (tk != ')') expr(Assign);
      while (tk == ',') {
         int *g = n; next(); expr(Assign); *--n = (int) g; *--n = '{';
      }
      b = n;
      if (tk != ')') fatal("close parentheses expected");
      next();
      ++brkc; ++cntc;
      stmt(ctx); c = n;
      --brkc; --cntc;
      *--n = (int) d; *--n = (int) c; *--n = (int) b; *--n = (int) a;
      *--n = For;
      return;
   case Goto:
      next();
      if (tk != Id || (id->type != 0 && id->type != -1)
                || (id->class != Label && id->class != 0))
         fatal("goto expects label");
      id->type = -1; // hack for id->class deficiency
      *--n = (int) id; *--n = Goto; next();
      if (tk != ';') fatal("semicolon expected");
      next();
      return;
   // stmt -> '{' stmt '}'
   case '{':
      next();
      *--n = ';';
      while (tk != '}') {
         a = n; check_label(&a); stmt(ctx); *--n = (int) a; *--n = '{';
      }
      next();
      return;
   // stmt -> ';'
   case ';':
      next();
      *--n = ';';
      return;
   default:
      expr(Assign);
      if (tk != ';' && tk != ',') fatal("semicolon expected");
      next();
   }
}

void die(char *msg) { printf("%s\n", msg); exit(-1); }

int reloc_imm(int offset) { return (((offset) - 8) >> 2) & 0x00ffffff; }
int reloc_bl(int offset) { return 0xeb000000 | reloc_imm(offset); }

int *codegen(int *jitmem, int *jitmap)
{
   int i, ii, jj, kk, tmp, c, ni, nf, sz, off;
   int align_stack, sp_odd = 0;
   int *je, *tje;    // current position in emitted native code
   int *immloc, *immlocv, *il, *ill, *ivv;
   int *rMap;

   immloc = il = malloc(1024 * 4);
   int *iv = malloc(1024 * 4);
   immlocv = iv;
   int *imm0 = 0;
   int *immf0 = 0;

   // first pass: emit native code
   int *pc = text + 1; je = jitmem; line = 0;
   while (pc <= e) {
      i = *pc;
      // Store mapping from IR index to native instruction buffer location
      // "pc - text" gets the index of IR.
      // "je" points to native instruction buffer's current location.
      if (peephole && i == ENT) while((int)je & 0x0f) *je++ = 0xe1a00000; // mov r0, r0
      jitmap[((int) pc++ - (int) text) >> 2] = (int) je;
      switch (i) {
      case LEA:
         tmp = *pc++;
         if (tmp >= 64 || tmp <= -64) {
            printf("jit: LEA %d out of bounds\n", tmp); exit(6);
         }
         if (tmp >= 0)
            *je++ = 0xe28b0000 | tmp * 4;     // add r0, fp, #(tmp)
         else
            *je++ = 0xe24b0000 | (-tmp) * 4;  // sub r0, fp, #(tmp)
         break;
      case IMM:
         tmp = *pc++;
         if (0 <= tmp && tmp < 256)
            *je++ = 0xe3a00000 + tmp;         // mov r0, #(tmp)
         else { if (!imm0) imm0 = je; *il++ = (int) (je++); *iv++ = tmp; }
         break;
      case IMMF:
         tmp = (int) *pc++;
         if (tmp == 0) *je++ = 0xee300a40 ; // sub s0, s0, s0
         else {
            if (!imm0) imm0 = je; if (!immf0) immf0 = je;
            *il++ = (int) je++ + 2; *iv++ = tmp;
         }
         break;
      case JMP:
      case JSR:
         pc++; je++; // postponed till second pass
         break;
      case BZ:
      case BNZ:
         *je++ = 0xe3500000; pc++; je++;      // cmp r0, #0
         break;
      case ENT:
         *je++ = 0xe92d4800; *je++ = 0xe28db000; // push {fp,lr}; add fp, sp, #0
         ii = c = 0; tmp = 4 * (*pc++);
         while (tmp >= 255) { c |= tmp & 3; tmp >>= 2; ++ii; }
         tmp += (c ? 1 : 0); if ((tmp << (2*ii)) >= 32768 || tmp < 0) {
            printf("jit: ENT %d out of bounds\n", tmp << (2*ii)); exit(6);
         } // sub  sp, sp, #tmp (scaled)
         if (tmp) *je++ = 0xe24dd000 | (((16-ii) & 0xf) << 8) | tmp;
         if (ii == 0 && (tmp & 4)) sp_odd = 1;
         if (peephole) { // reserve space for frame registers
            for (ii=0; ii<8; ++ii) *je++ = 0xe1a00000; // mov r0, r0
         }
         break;
      case ADJ:
         *je++ = 0xe28dd000 + ( *pc++ & 0xf) * 4; // add sp, sp, #(tmp * 4)
         break;
      case LEV:
         *je++ = 0xe28bd000; *je++ = 0xe8bd8800; // add sp, fp, #0; pop {fp, pc}
         break;
      case PSH:
         *je++ = 0xe52d0004;                     // push {r0}
         break;
      case PSHF:
         *je++ = 0xed2d0a01;                     // push {s0}
         break;
      case LC:
         *je++ = 0xe5d00000; if (signed_char)  *je++ = 0xe6af0070; // ldrb r0, [r0]
         break;
      case LI:
         *je++ = 0xe5900000;                     // ldr r0, [r0]
         break;
      case LF:
         *je++ = 0xed900a00;                     // vldr s0, [r0]
         break;
      case SC:
         *je++ = 0xe49d1004; *je++ = 0xe5c10000; // pop {r1}; strb r0, [r1]
         break;
      case SI:
         *je++ = 0xe49d1004; *je++ = 0xe5810000; // pop {r1}; str r0, [r1]
         break;
      case SF:
         *je++ = 0xe49d1004; *je++ = 0xed810a00; // pop {r1}; vstr s0, [r1]
         break;
      case OR:
         *je++ = 0xe49d1004; *je++ = 0xe1810000; // pop {r1}; orr r0, r1, r0
         break;
      case XOR:
         *je++ = 0xe49d1004; *je++ = 0xe0210000; // pop {r1}; eor r0, r1, r0
         break;
      case AND:
         *je++ = 0xe49d1004; *je++ = 0xe0010000; // pop {r1}; and r0, r1, r0
         break;
      case SHL:
         *je++ = 0xe49d1004; *je++ = 0xe1a00011; // pop {r1}; lsl r0, r1, r0
         break;
      case SHR:
         *je++ = 0xe49d1004; *je++ = 0xe1a00051; // pop {r1}; asr r0, r1, r0
         break;
      case ADD:
         *je++ = 0xe49d1004; *je++ = 0xe0810000; // pop {r1}; add r0, r1, r0
         break;
      case SUB:
         *je++ = 0xe49d1004; *je++ = 0xe0410000; // pop {r1}; sub r0, r1, r0
         break;
      case MUL:
         *je++ = 0xe49d1004; *je++ = 0xe0000091; // pop {r1}; mul r0, r1, r0
         break;
      case DIV:
      case MOD:
         if (peephole) *je++ = 0xe1a01001;      // mov r1, r1
         *je++ = 0xe52d0004;                    // push {r0}
         int ti = ef_getidx((i == DIV) ? "__aeabi_idiv" : "__aeabi_idivmod");
         tmp = ef_getaddr(ti);
         if (peephole) *je++ = 0xe1a01001;      // mov r1, r1
         *je++ = 0xe49d0004 | (1 << 12);        // pop r1
         if (peephole) *je++ = 0xe1a01001;      // mov r1, r1
         *je++ = 0xe49d0004 | (0 << 12);        // pop r0
         if (peephole) *je++ = 0xe1a01001;      // mov r1, r1
         *je++ = 0xe28fe000;                    // add lr, pc, #0
         if (!imm0) imm0 = je; *il++ = (int) je++ + 1; *iv++ = tmp;
         // ARM EABI modulo helper function produces quotient in r0
         // and the remainder in r1.
         if (i == MOD) {
            if (peephole) *je++ = 0xe1a01001;   // mov r1, r1
            *je++ = 0xe1a00001;                 // mov r0, r1
         }
         break;
      case ADDF:
         *je++ = 0xecfd0a01; *je++ = 0xee300a80; // pop {s1}; add s0, s1, s0
         break;
      case SUBF:
         *je++ = 0xecfd0a01; *je++ = 0xee300ac0; // pop {s1}; sub s0, s1, s0
         break;
      case MULF:
         *je++ = 0xecfd0a01; *je++ = 0xee200a80; // pop {s1}; mul s0, s1, s0
         break;
      case DIVF:
         *je++ = 0xecfd0a01; *je++ = 0xee800a80; // pop {s1}; div s0, s1, s0
         break;
      case FTOI:
         *je++ = 0xeefd0a40; *je++ = 0xee100a90; // ftosis s1, s0; fmrs r0, s1
         break;
      case ITOF:
         *je++ = 0xee000a90; *je++ = 0xeeb80ae0; // frms s1, r0; fsitos s0, s1
         break;
      case SYSC:
         if (pc[1] != ADJ) die("codegen: no ADJ after native proc");
         if ((pc[2] & 0xf) > 10) die("codegen: no support for 11+ arguments");
         c = pc[2]; ii = c & 0xf; c >>= 4; nf = c & 0xf; ni = ii - nf; c >>= 4;
         tmp = ef_getaddr(*pc);  // look up address from ef index

         // Handle ridiculous printf() ABI inline
         int isPrtf = nf && !strcmp(ef_cache[*pc]->name, "printf");
         if (isPrtf) {
            if (ii == 0) die("printf() has no arguments");
            jj = ii - 1; sz = 0; off = 0; rMap = (int *) malloc(ii*sizeof(int));
            do {
               if (c & (1 << jj)) { // float (adjust as though a double)
                  sz = sz + (sz & 1);
                  rMap[jj] = sz;
                  sz = sz + 2;
               }
               else {
                  rMap[jj] = sz;
                  sz = sz + 1;
               }
               --jj;
            } while (jj >= 0);
            sz = sz + (sz & 1);
            // instead?:  test sp, #4; subeq sp, sp, 4; eoreq lr, 0x80000000
            align_stack = (sp_odd ^ (ii & 1)); // sp must be 8-byte aligned
            if (sz > 4 || align_stack) {
               if (align_stack) off = 4;
               if (sz > 4) off += 4*(sz-4);
               *je++ = 0xe24dd000 | off; // sub sp, sp, #off;
            }
            jj = 0; // in case arguments are all in regs
            if (sz > 4) { // set up stack arguments
               jj = ii - 1;
               do {
                  if (rMap[jj] == 4)
                     break;
                  --jj;
               } while (jj >= 0);
               kk = jj + 1; // number of arguments to copy
               for (jj = 0; jj < kk; ++jj) {
                  if (c & (1 << jj)) { // float
                     // vldr s1, [sp, #(sz + jj*4)]
                     // fcvtds d0, s1
                     // vstr d0,[sp, #rMap[jj]-4]
                     *je++ = 0xeddd0a00 | (jj + off/4);
                     *je++ = 0xeeb70ae0;
                     *je++ = 0xed8d0b00 | (rMap[jj]-4);
                  }
                  else { // int
                     // ldr r0, [sp, #(sz + jj*4)]
                     // str r0, [sp, #rMap[jj]-4]
                     *je++ = 0xe59d0000 | (off + jj*4);
                     *je++ = 0xe58d0000 | (rMap[jj]*4-16);
                  }
               }
            }
            for(; jj < ii; ++jj) { // handle values in regs
               if (c & (1 << jj)) { // float
                  // vldr s"rMap[jj]+1", [sp, #(sz + jj*4)]
                  // fcvtds d"rMap[jj]/2", s"rMap[jj]+1"
                  // vmov r"rMap[jj]", r"rMap[jj]+1", d"rMap[jj]/2"
                  *je++ = 0xeddd0a00 | (rMap[jj] << 11) | (jj + off/4);
                  *je++ = 0xeeb70ae0 | (rMap[jj] << 11) | (rMap[jj] >> 1);
                  if (peephole) *je++ = 0xe1a01001;  // mov r1, r1
                  *je++ = 0xec500b10 | (rMap[jj] << 12) |
                          ((rMap[jj] + 1) << 16) | (rMap[jj] >> 1);
               }
               else { // int
                  if (peephole) *je++ = 0xe1a01001;  // mov r1, r1
                  // ldr r"rMap[jj]", [sp, #(sz + jj*4)]
                  *je++ = 0xe59d0000 | (rMap[jj] << 12) | (off + jj*4);
               }
            }
            ++pc; // point at ADJ instruction
         }
         else {
            pc = pc + 3; kk = ni;
            for (jj = 0; jj < ii; ++jj) {
               if (peephole) *je++ = 0xe1a01001;  // mov r1, r1
               if (c & (1 << jj)) { // vpop {nf}
                  --nf;
                  *je++ = 0xecbd0a01 | ((nf & 1) << 22) | ((nf & 0xe) << 11);
               }
               else // pop {ni}
                  *je++ = 0xe49d0004 | (--ni << 12); // pop {ni}
            }
            if ((ni = kk) > 4) {
               if (peephole) *je++ = 0xe1a01001;  // mov r1, r1
               *je++ = 0xe92d03f0;
            }
         }
         if (peephole) *je++ = 0xe1a01001;     // mov r1, r1
         *je++ = 0xe28fe000;                   // add lr, pc, #0
         if (!imm0) imm0 = je; *il++ = (int) je++ + 1; *iv++ = tmp;
         if (isPrtf) {
            // instead?  tests lr, 0; addpl sp, sp, #4
            if (sz > 4 || align_stack) {
               *je++ = 0xe28dd000 | off; // add sp, sp, #off;
            }
            free(rMap);
         }
         else {
            if (ni > 4) {
               if (peephole) *je++ = 0xe1a01001;     // mov r1, r1
               *je++ = 0xe28dd018;                   // add sp, sp, #24
            }
         }
         break;
      case CLCA:
         *je++ = 0xe59d0004; *je++ = 0xe59d1000; // ldr r0, [sp, #4]
                                                 // ldr r1, [sp]
         *je++ = 0xe3a0780f; *je++ = 0xe2877002; // mov r7, #0xf0000
                                                 // add r7, r7, #2
         *je++ = 0xe3a02000; *je++ = 0xef000000; // mov r2, #0
                                                 // svc 0
         break;
      case SQRT: *je++ = 0xeeb10ac0; break;      // fsqrts s0, s0
      case PHD:  *je++ = 0xe1a01001; break;      // mov r1, r1
      case PHR0: *je++ = 0xe1a0d00d; break;      // mov sp, sp
      default:
         if ((EQ <= i && i <= LE) || (EQF <= i && i <= LEF)) {
            if (EQ <= i && i <= LE) {
               *je++ = 0xe49d1004; *je++ = 0xe1510000; // pop {r1}; cmp r1, r0
            }
            else {
               *je++ = 0xecfd0a01; *je++ = 0xeef40ac0; // pop {s1}; vcmpe s1, s0
               *je++ = 0xeef1fa10; i -= (EQF - EQ);    // vmrs APSR_nzcv, fpscr
               if (*pc == FTOI) *pc = PHR0;
            }
            if (i <= NE) { je[0] = 0x03a00000; je[1] = 0x13a00000; }   // moveq r0, #0; movne r0, #0
            else if (i <= LT) { je[0] = 0xb3a00000; je[1] = 0xa3a00000; } // movlt r0, #0; movge   r0, #0
            else { je[0] = 0xc3a00000; je[1] = 0xd3a00000; }           // movgt r0, #0; movle r0, #0
            if (i == EQ || i == LT || i == GT) je[0] = je[0] | 1;
            else je[1] = je[1] | 1;
            je += 2;
            break;
         } else {
            printf("code generation failed for %d!\n", i);
            free(iv);
            return 0;
         }
      }

      int genpool = 0;
      if (imm0) {
         if (i == LEV || i == JMP) genpool = 1;
         else if ( ((int) je > (int) imm0 + 3072) || // pad for optimizer
                   (immf0 && ((int) je > (int) immf0 + 512)) ) {
            tje = je; --tje; // workaround for "*(ptr - literal)" bug
            if (*tje != 0xe1a01001) { // NOP : mov r1, r1
               tje = je++; genpool = 2;
            }
         }
      }
      if (genpool) {
         ill = immloc;
         ivv = immlocv;
         *iv = 0;
         while (ill < il) {
            tmp = *ill++;
            if ((int) je > tmp + 4096 + 8) die("codegen: can't reach the pool");
            if (tmp & 1) {
               // ldr pc, [pc, #..]
               --tmp; *((int *) tmp) = 0xe59ff000 | ((int) je - tmp - 8);
            } else if (tmp & 2) {
               tmp -= 2;
               if ((int) je > (tmp + 255*4 + 8))
                  die("codegen: float constant too far");
               // vldr s0, [pc, #..]
               *((int *) tmp) = 0xed9f0a00 | (((int) je - tmp - 8) >> 2);
            } else {
               // ldr r0, [pc, #..]
               *(int *) tmp = 0xe59f0000 | ((int) je - tmp - 8);
            }
            *je++ = *ivv;
            if (ivv[0] == ivv[1]) --je; ++ivv;
         }
         if (genpool == 2) { // jump past the pool
            tmp = ((int) je - (int) tje - 8) >> 2;
            *tje = 0xea000000 | (tmp & 0x00ffffff); // b #(je)
         }
         imm0 = 0; immf0 = 0; genpool = 0;
         iv = immlocv; il = immloc;
      }
   }
   if (il > immloc) die("codegen: not terminated by a LEV");
   tje = je;

   // second pass
   pc = text + 1; // Point instruction pointer "pc" to the first instruction.
   while (pc <= e) { // While instruction end is not met.
      // Get the IR's corresponding native instruction buffer address.
      je = (int *) jitmap[((int) pc - (int) text) >> 2];
      i = *pc++; // Get current instruction
      // If the instruction is one of the jumps.
      if (i >= JMP && i <= BNZ) {
         switch (i) {
         case JMP: *je = 0xea000000;   break; // b #(tmp)
         case JSR: *je = 0xeb000000;   break; // bl #(tmp)
         case BZ:  *++je = 0x0a000000; break; // beq #(tmp)
         case BNZ: *++je = 0x1a000000; break; // bne #(tmp)
         }
         tmp = *pc++;
         *je = (*je | reloc_imm(jitmap[(tmp - (int) text) >> 2] - (int) je));
      }
      // If the instruction has operand, increment instruction pointer to
      // skip he operand.
      else if (i <= ADJ || i == SYSC) { ++pc; }
   }
   free(iv);
   return tje;
}

enum {
   _PROT_EXEC = 4, _PROT_READ = 1, _PROT_WRITE = 2,
   _MAP_PRIVATE = 2, _MAP_ANON = 32
};

int jit(int poolsz, int *main, int argc, char **argv)
{
   char *jitmem;  // executable memory for JIT-compiled native code
   int retval;
   if (src) return 0; // skip for IR listing

   // setup JIT memory
   if (!(jitmem = mmap(0, poolsz, _PROT_EXEC | _PROT_READ | _PROT_WRITE,
                  _MAP_PRIVATE | _MAP_ANON, -1, 0))) {
      printf("could not mmap(%d) jit executable memory\n", poolsz);
      return -1;
   }
   int *jitmap = (int *) (jitmem + (poolsz >> 1));
   int *je = (int *) jitmem;
   *je++ = (int) &retval;
   *je++ = argc;
   *je++ = (int) argv;
   int *_start = je;
   *je++ = 0xe92d5ff0;      // push    {r4-r12, lr}
   *je++ = 0xe51f0014;      // ldr     r0, [pc, #-20] ; argc
   *je++ = 0xe51f1014;      // ldr     r1, [pc, #-20] ; argv
   *je++ = 0xe52d0004;      // push    {r0}
   *je++ = 0xe52d1004;      // push    {r1}
   int *tje = je++;         // bl      jitmain
   *je++ = 0xe51f502c;      // ldr     r5, [pc, #-44] ; retval
   *je++ = 0xe5850000;      // str     r0, [r5]
   *je++ = 0xe28dd008;      // add     sp, sp, #8
   *je++ = 0xe8bd9ff0;      // pop     {r4-r12, pc}
   if (!(je = codegen(je, jitmap))) return 1;
   if (je >= jitmap) die("jitmem too small");
   *tje = reloc_bl(jitmap[((int) main - (int) text) >> 2] - (int) tje);

#ifdef SQUINT_SO
   if (peephole) squint_opt((int *)jitmem, je);
#endif

   // hack to jump into specific function pointer
   __clear_cache(jitmem, je);
   int *res = bsearch(&sym, sym, 1, 1, (void *) _start);
   if (((void *) 0) != res) return 0; return -1; // make compiler happy
}

int ELF32_ST_INFO(int b, int t) { return (b << 4) + (t & 0xf); }
enum {
   EHDR_SIZE = 52, ET_EXEC = 2, EM_ARM = 40,
   PHDR_ENT_SIZE = 32, SHDR_ENT_SIZE = 40,
   SYM_ENT_SIZE = 16, REL_ENT_SIZE = 8, PLT_ENT_SIZE = 12,
   DYN_ENT_SIZE = 8
};

struct Elf32_Shdr {
   int sh_name;      // [Elf32_Word] Section name (index into string table)
   int sh_type;      // [Elf32_Word] Section type (SHT_*)
   int sh_flags;     // [Elf32_Word] Section flags (SHF_*)
   int sh_addr;      // [Elf32_Addr] Address where section is to be loaded
   int sh_offset;    // [Elf32_Off] File offset of section data, in bytes
   int sh_size;      // [Elf32_Word] Size of section, in bytes
   int sh_link;      // [Elf32_Word] Section type-specific header table
                     //              index link
   int sh_info;      // [Elf32_Word] Section type-specific extra information
   int sh_addralign; // [Elf32_Word] Section address alignment
   int sh_entsize;   // [Elf32_Word] Size of records contained within section
};

enum {
   // Special section indices
   SHN_UNDEF        = 0,  // Undefined, missing, irrelevant, or meaningless

   // Section types
   SHT_NULL         = 0,  // No associated section (inactive entry)
   SHT_PROGBITS     = 1,  // Program-defined contents
   SHT_STRTAB       = 3,  // String table
   SHT_DYNAMIC      = 6,  // Information for dynamic linking
   SHT_REL          = 9,  // Relocation entries; no explicit addends
   SHT_DYNSYM       = 11, // Symbol table

   // Section flags
   SHF_WRITE = 0x1,
   SHF_ALLOC = 0x2,
   SHF_EXECINSTR = 0x4,
};

// Symbol table entries for ELF32
struct Elf32_Sym {
   int st_name;  // [Elf32_Word] Symbol name (index into string table)
   int st_value; // [Elf32_Addr] Value or address associated with the symbol
   int st_size;  // [Elf32_Word] Size of the symbol
   char st_info; // [unsigned] Symbol's type and binding attributes
   char st_other;// [unsigned] Must be zero; reserved
   char st_shndx, st_shndx_1, st_shndx_2, st_shndx_3; // [Elf32_Half]
                 // Which section (header table index) it's defined
};

enum {
   // Symbol bindings
   STB_LOCAL  = 0,  /* Local symbol, not visible outside obj file
                                containing def */
   STB_GLOBAL = 1,  /* Global symbol, visible to all object files
                               being combined */

   // Symbol types
   STT_NOTYPE  = 0,   // Symbol's type is not specified
   STT_FUNC    = 2,   // Symbol is executable code (function, etc.)

   // Symbol number
   STN_UNDEF = 0
};

// Program header for ELF32
struct Elf32_Phdr {
   int p_type;   // [Elf32_Word] Type of segment
   int p_offset; // [Elf32_Off] File offset where segment is located, in bytes
   int p_vaddr;  // [Elf32_Addr] Virtual address of beginning of segment
   int p_paddr;  // [Elf32_Addr] Physical address of beginning of segment
                 //              (OS-specific)
   int p_filesz; // [Elf32_Word] Number of bytes in file image of segment
                 //              (may be zero)
   int p_memsz;  // [Elf32_Word] Number of bytes in mem image of segment
                 //              (may be zero)
   int p_flags;  // [Elf32_Word] Segment flags
   int p_align;  // [Elf32_Word] Segment alignment constraint
};

// Segment types
enum {
   PT_NULL    = 0, // Unused segment
   PT_LOAD    = 1, // Loadable segment
   PT_DYNAMIC = 2, // Dynamic linking information
   PT_INTERP  = 3, // Interpreter pathname

   // Segment flag bits
   PF_X       = 1,         // Execute
   PF_W       = 2,         // Write
   PF_R       = 4,         // Read
};

int phdr_idx, shdr_idx, sym_idx;

int gen_phdr(char *ptr, int type, int offset, int addr, int size,
          int flag, int align)
{
   struct Elf32_Phdr *phdr = (struct Elf32_Phdr *) ptr;
   phdr->p_type =  type;
   phdr->p_offset = offset;
   phdr->p_vaddr = addr;
   phdr->p_paddr = addr;
   phdr->p_filesz = size;
   phdr->p_memsz = size;
   phdr->p_flags = flag;
   phdr->p_align = align;
   return phdr_idx++;
}

int gen_shdr(char *ptr, int type, int name, int offset, int addr,
          int size, int link, int info,
          int flag, int align, int entsize)
{
   struct Elf32_Shdr *shdr = (struct Elf32_Shdr *) ptr;
   shdr->sh_name = name;      shdr->sh_type = type;
   shdr->sh_addr = addr;      shdr->sh_offset = offset;
   shdr->sh_size = size;      shdr->sh_link = link;
   shdr->sh_info = info;      shdr->sh_flags = flag;
   shdr->sh_addralign = align; shdr->sh_entsize = entsize;
   return shdr_idx++;
}

int gen_sym(char *ptr, int name, char info,
         int shndx, int size, int value)
{
   struct Elf32_Sym *s = (struct Elf32_Sym *) ptr;
   s->st_name = name;
   s->st_info = info;
   s->st_other = 0;
   // s->st_shndx = shndx;
   memcpy(&(s->st_shndx), (char *) &shndx, 2);
   s->st_value = value;
   s->st_size = size;
   return sym_idx++;
}

int append_func_sym(char **sdata, int name)
{
   int idx = gen_sym(*sdata, name, ELF32_ST_INFO(STB_GLOBAL, STT_FUNC), 0, 0, 0);
   *sdata += SYM_ENT_SIZE;
   return idx;
}

// shdr names which start with 'S'
enum {
   SNONE = 0, SSTAB, STEXT, SDATA, SDYNS, SDYNM, SDYNC,
   SINTP, SREL, SPLT, SGOT
};

enum {
   PAGE_SIZE = 0x1000, PHDR_NUM = 4, SHDR_NUM = 11,
   DYN_NUM = 16
};

void elf32_init(int poolsz)
{
   int i;
   freebuf = malloc(poolsz);
   char *o = (char *) (((int) freebuf + PAGE_SIZE - 1)  & -PAGE_SIZE);
   /* We must assign the plt_func_addr[x] a non-zero value, and also,
    * plt_func_addr[i] and plt_func_addr[i-1] has an offset of 16
    * (4 instruction * 4 bytes), so the first codegen and second codegen
    * have consistent code_size. Dummy address at this point.
    */
   plt_func_addr = malloc(sizeof(char *) * PTR);
   for (i = 0; i < PTR; ++i)
      plt_func_addr[i] = o + i * 16;

   ef_getidx("__libc_start_main"); // slot 0 of external func cache
}

int elf32(int poolsz, int *main, int elf_fd)
{
   int i;
   char *freecode;
   char *code = freecode = malloc(poolsz);
   char *buf = freebuf;
   int *jitmap = (int *) (code + (poolsz >> 1));
   memset(buf, 0, poolsz);
   char *o = buf = (char *) (((int) buf + PAGE_SIZE - 1)  & -PAGE_SIZE);
   code = (char *) (((int) code + PAGE_SIZE - 1) & -PAGE_SIZE);

   phdr_idx = 0;
   shdr_idx = 0;
   sym_idx = 0;

   /* Run __libc_start_main() and pass main trampoline.
    *
    * Note: The function prototype of __libc_start_main() is:
    *
    *     int __libc_start_main(int (*main)(int, char**, char**),
    *                      int argc, char **argv,
    *                      int (*init)(int, char**, char**),
    *                      void (*fini)(void),
    *                      void (*rtld_fini)(void),
    *                      void *stack_end);
    *
    * Usually, we should pass __libc_csu_init as init and __libc_csu_fini
    * as fini; however, we will need a interp to link the non-shared part
    * of libc.  It sounds too complex.  To keep this compiler simple,
    * let's simply pass NULL pointer.
    */
   int *stub_end = (int *) code;

   *stub_end++ = 0xe3a0b000;  // mov   fp, #0  @ initialize frame pointer
   *stub_end++ = 0xe3a0e000;  // mov   lr, #0  @ initialize link register
   *stub_end++ = 0xe49d1004;  // pop   {r1}    @ get argc
   *stub_end++ = 0xe1a0200d;  // mov   r2, sp  @ get argv
   *stub_end++ = 0xe52d2004;  // push  {r2}    @ setup stack end
   *stub_end++ = 0xe52d0004;  // push  {r0}    @ setup rtld_fini
   *stub_end++ = 0xe3a0c000;  // mov   ip, #0  @ FIXME: __libc_csu_fini()
   *stub_end++ = 0xe52dc004;  // push  {ip}    @ setup fini
   *stub_end++ = 0xe28f0010;  // add   r0, pc, #16  @ load main trampoline
   *stub_end++ = 0xe3a03000;  // mov   r3, #0  @ FIXME: __libc_csu_init()
   *stub_end++ = 0xebfffffe;  // bl    __libc_start_main  @ Need relocation

   // Return 127 if __libc_start_main() returns (which should not.)
   *stub_end++ = 0xe3a0007f;  // mov   r0, #127
   *stub_end++ = 0xe3a07001;  // mov   r7, #1
   *stub_end++ = 0xef000000;  // svc   0x00000000

   // main() trampoline: convert ARM AAPCS calling convention to ours.
   *stub_end++ = 0xe92d5ff0;  // push  {r4-r12, lr}
   *stub_end++ = 0xe52d0004;  // push  {r0}
   *stub_end++ = 0xe52d1004;  // push  {r1}
   *stub_end++ = 0xebfffffe;  // bl    0 <main>  @ Need relocation
   *stub_end++ = 0xe28dd008;  // add   sp, sp, #8
   *stub_end++ = 0xe8bd9ff0;  // pop   {r4-r12, pc}

   int start_stub_size = (char *) stub_end - code;

   // Compile and generate the code.
   char *je = (char *) codegen((int *) (code + start_stub_size), jitmap);
   if (!je) return 1;
   if ((int *) je >= jitmap) die("elf32: jitmem too small");

#ifdef SQUINT_SO
   if (peephole) squint_opt((int *)code, (int *) je);
#endif

   // elf32_hdr
   *o++ = 0x7f; *o++ = 'E'; *o++ = 'L'; *o++ = 'F';
   *o++ = 1;    *o++ = 1;   *o++ = 1;   *o++ = 0;
   o += 8;
   *o++ = ET_EXEC;        *o++ = 0; // e_type
   *o++ = EM_ARM;         *o++ = 0; // e_machine
   *(int *) o = 1;          o += 4;
   char *entry = o;         o += 4; // e_entry
   *(int *) o = EHDR_SIZE;  o += 4; // e_phoff
   char *e_shoff = o;       o += 4; // e_shoff
   *(int *) o = 0x5000400;  o += 4; // e_flags
   *o++ = EHDR_SIZE; *o++ = 0;
   *o++ = PHDR_ENT_SIZE; *o++ = 0; *o++ = PHDR_NUM; *o++ = 0; // e_phentsize & e_phnum
   *o++ = SHDR_ENT_SIZE; *o++ = 0; *o++ = SHDR_NUM; *o++ = 0; // e_shentsize & e_shnum
   *o++ =  1; *o++ = 0;

   int phdr_size = PHDR_ENT_SIZE * PHDR_NUM;
   char *phdr = o; o += phdr_size;

   // .text
   int code_off = o - buf;
   int code_size = je - code;
   char *code_addr = o;
   o += code_size;

   // .rel.plt (embedded in PT_LOAD of text)
   int rel_size = REL_ENT_SIZE * ef_count;
   int rel_off = code_off + code_size;
   char *rel_addr = code_addr + code_size;
   o += rel_size;

   // .plt (embedded in PT_LOAD of text)
   // 20 is the size of .plt entry code to .got
   int plt_size = 20 + PLT_ENT_SIZE * ef_count;
   int plt_off = rel_off + rel_size;
   char *plt_addr = rel_addr + rel_size;
   o += plt_size;

   memcpy(code_addr, code,  code_size);
   *(int *) entry = (int) code_addr;

   // .data
   char *_data_end = data;
   // Use load_bias to align offset and v_addr, the elf loader
   // needs PAGE_SIZE align to do mmap().
   int load_bias = PAGE_SIZE + ((int) _data & (PAGE_SIZE - 1))
                               - ((o - buf) & (PAGE_SIZE - 1));
   o += load_bias;
   char *dseg = o;

   // rwdata (embedded in PT_LOAD of data)
   // rwdata is all the data (R/O and R/W) in source code,
   // e.g, the variable with initial value and all the string.
   int rwdata_off = dseg - buf;
   int rwdata_size = _data_end - _data;
   o += rwdata_size;

   // .dynamic (embedded in PT_LOAD of data)
   char *pt_dyn = data;
   int pt_dyn_size = DYN_NUM * DYN_ENT_SIZE;
   int pt_dyn_off = rwdata_off + rwdata_size; data += pt_dyn_size;
   o += pt_dyn_size;

   // .interp (embedded in PT_LOAD of data)
   char *interp_str = "/lib/ld-linux-armhf.so.3";
   int interp_str_size = 25; // strlen(interp_str) + 1
   char *interp = data;
   memcpy(interp, interp_str, interp_str_size);
   int interp_off = pt_dyn_off + pt_dyn_size; data += interp_str_size;
   o += interp_str_size;

   // .shstrtab (embedded in PT_LOAD of data)
   char *shstrtab_addr = data;
   int shstrtab_off = interp_off + interp_str_size;
   int shstrtab_size = 0;

   int *shdr_names = malloc(sizeof(int) * SHDR_NUM);
   if (!shdr_names) die("elf32: could not malloc shdr_names table");

   shdr_names[SNONE] = append_strtab(&data, "") - shstrtab_addr;
   shdr_names[SSTAB] = append_strtab(&data, ".shstrtab") - shstrtab_addr;
   shdr_names[STEXT] = append_strtab(&data, ".text") - shstrtab_addr;
   shdr_names[SDATA] = append_strtab(&data, ".data") - shstrtab_addr;
   shdr_names[SDYNS] = append_strtab(&data, ".dynstr") - shstrtab_addr;
   shdr_names[SDYNM] = append_strtab(&data, ".dynsym") - shstrtab_addr;
   shdr_names[SDYNC] = append_strtab(&data, ".dynamic") - shstrtab_addr;
   shdr_names[SINTP] = append_strtab(&data, ".interp") - shstrtab_addr;
   shdr_names[SREL] = append_strtab(&data, ".rel.plt") - shstrtab_addr;
   shdr_names[SPLT] = append_strtab(&data, ".plt") - shstrtab_addr;
   shdr_names[SGOT] = append_strtab(&data, ".got") - shstrtab_addr;
   shstrtab_size = data - shstrtab_addr;
   o += shstrtab_size;

   // .dynstr (embedded in PT_LOAD of data)
   char *dynstr_addr = data;
   int dynstr_off = shstrtab_off + shstrtab_size;
   append_strtab(&data, "");
   char *libc = append_strtab(&data, "libc.so.6");
   char *ldso = append_strtab(&data, "libdl.so.2");
   char *libgcc_s = append_strtab(&data, "libgcc_s.so.1");
   char *libm = append_strtab(&data, "libm.so.6");

   int *func_entries = malloc(sizeof(int) * ef_count);
   if (!func_entries) die("elf32: could not malloc func_entries table");

   for (i = 0; i < ef_count; ++i)
      func_entries[i] = append_strtab(&data, ef_cache[i]->name) - dynstr_addr;

   int dynstr_size = data - dynstr_addr;
   o += dynstr_size;

   // .dynsym (embedded in PT_LOAD of data)
   char *dynsym_addr = data;
   int dynsym_off = dynstr_off + dynstr_size;
   memset(data, 0, SYM_ENT_SIZE);
   data += SYM_ENT_SIZE;

   for (i = 0; i < ef_count; ++i)
      append_func_sym(&data, func_entries[i]);

   int dynsym_size = SYM_ENT_SIZE * (ef_count + 1);
   o += dynsym_size;

   // .got (embedded in PT_LOAD of data)
   char *got_addr = data;
   int got_off = dynsym_off + dynsym_size;
   *(int *) data = (int) pt_dyn; data += 4;
   data += 4;                 // reserved 2 and 3 entry for interp
   char *to_got_movw = data;  // The address manipulates dynamic
   char *to_got_movt = data;  // linking, plt must jump here.
   data += 4;                 // reserved 2 and 3 entry for interp
   // .got function slot
   char **got_func_slot = malloc(sizeof(char *) * ef_count);
   for (i = 0; i < ef_count; ++i) {
      got_func_slot[i] = data;
      *(int *) data = (int) plt_addr; data += 4;
   }
   data += 4;  // end with 0x0
   int got_size = (int) data - (int) got_addr;
   o += got_size;

   int dseg_size = o - dseg;

   // .plt -- Now we back to handle .plt after .got was initial
   char *to = plt_addr;
   *(int *) to = 0xe52de004; to += 4; // push {lr}
   // movw r10 addr_to_got
   *(int *) to = 0xe300a000 | (0xfff & (int) (to_got_movw)) |
                   (0xf0000 & ((int) (to_got_movw) << 4));
   to += 4;
   // movt r10 addr_to_got
   *(int *) to = 0xe340a000 | (0xfff & ((int) (to_got_movt) >> 16)) |
                   (0xf0000 & ((int) (to_got_movt) >> 12));
   to += 4;
   *(int *) to = 0xe1a0e00a; to += 4;  // mov lr,r10
   *(int *) to = 0xe59ef000; to += 4;  // ldr pc, [lr]

   // We must preserve ip for code below, dyn link use this as return address
   for (i = 0; i < ef_count; ++i) {
      plt_func_addr[i] = to;
      // movt ip addr_to_got
      *(int *) to = 0xe300c000 | (0xfff & (int) (got_func_slot[i])) |
                      (0xf0000 & ((int) (got_func_slot[i]) << 4));
      to += 4;
      // movw ip addr_to_got
      *(int *) to = 0xe340c000 |
                      (0xfff & ((int) (got_func_slot[i]) >> 16)) |
                      (0xf0000 & ((int) (got_func_slot[i]) >> 12));
      to += 4;
      *(int *) to = 0xe59cf000; to += 4;  // ldr pc, [ip]
   }

   // .rel.plt
   to = rel_addr;
   for (i = 0; i < ef_count; ++i) {
      *(int *) to = (int) got_func_slot[i]; to += 4;
      *(int *) to = 0x16 | ((i + 1) << 8) ; to += 4;
      // 0x16 R_ARM_JUMP_SLOT | .dymstr index << 8
   }

   // Generate program header after we got address, offset and size.
   to = phdr;
   // PT_LOAD for .text
   gen_phdr(to, PT_LOAD, 0, (int) buf,
         EHDR_SIZE + phdr_size + code_size + rel_size + plt_size,
         PF_X | PF_R, PAGE_SIZE);
   to += PHDR_ENT_SIZE;

   // PT_LOAD for .data
   gen_phdr(to, PT_LOAD, rwdata_off, (int) _data,
         dseg_size, PF_W | PF_R, PAGE_SIZE);
   to += PHDR_ENT_SIZE;

   // PT_INTERP for .interp
   gen_phdr(to, PT_INTERP, interp_off, (int) interp,
         interp_str_size , PF_R, 0x1);
   to += PHDR_ENT_SIZE;

   // PT_DYNAMIC for .dynamic
   gen_phdr(to, PT_DYNAMIC, pt_dyn_off, (int) pt_dyn,
         pt_dyn_size , PF_R | PF_W, 0x4);

   // .dynamic (embedded in PT_LOAD of data)
   to = pt_dyn;
   *(int *) to =  5; to += 4; *(int *) to = (int) dynstr_addr;  to += 4;
   *(int *) to = 10; to += 4; *(int *) to = dynstr_size;        to += 4;
   *(int *) to =  6; to += 4; *(int *) to = (int) dynsym_addr;  to += 4;
   *(int *) to = 11; to += 4; *(int *) to = 16;                 to += 4;
   *(int *) to = 17; to += 4; *(int *) to = (int) rel_addr;     to += 4;
   *(int *) to = 18; to += 4; *(int *) to = rel_size;           to += 4;
   *(int *) to = 19; to += 4; *(int *) to = 8;                  to += 4;
   *(int *) to =  3; to += 4; *(int *) to = (int) got_addr;     to += 4;
   *(int *) to =  2; to += 4; *(int *) to = rel_size;           to += 4;
   *(int *) to = 20; to += 4; *(int *) to = 17;                 to += 4;
   *(int *) to = 23; to += 4; *(int *) to = (int) rel_addr;     to += 4;
   *(int *) to =  1; to += 4; *(int *) to = libc - dynstr_addr; to += 4;
   *(int *) to =  1; to += 4; *(int *) to = ldso - dynstr_addr; to += 4;
   *(int *) to =  1; to += 4; *(int *) to = libgcc_s - dynstr_addr; to += 4;
   *(int *) to =  1; to += 4; *(int *) to = libm - dynstr_addr; to += 4;
   *(int *) to =  0;

   /* Generate code again bacause address of .plt function slots must
    * be confirmed before codegen() to make sure the code is correct.
    */
   je = (char *) codegen((int *) (code + start_stub_size), jitmap);
   if (!je) {
      free(func_entries);
      free(shdr_names);
      return 1;
   }
   if ((int *) je >= jitmap) die("elf32: jitmem too small");

#ifdef SQUINT_SO
   if (peephole) squint_opt((int *)code, (int *) je);
#endif

   // Relocate __libc_start_main() and main().
   *((int *) (code + 0x28)) = reloc_bl(plt_func_addr[0] - code_addr - 0x28);
   *((int *) (code + 0x44)) =
      reloc_bl(jitmap[((int) main - (int) text) >> 2] - (int) code - 0x44);

   // Copy the generated binary.
   memcpy(code_addr, code,  je - code);

   // Generate section header
   *(int *) e_shoff = (int) (o - buf);
   gen_shdr(o, SHT_NULL, shdr_names[SNONE], 0, 0, 0,
          0, 0, 0, 0, 0);
   o += SHDR_ENT_SIZE;

   // sh_shstrtab_idx
   gen_shdr(o, SHT_STRTAB, shdr_names[SSTAB], shstrtab_off, 0,
          shstrtab_size, 0, 0, 0, 1, 0);
   o += SHDR_ENT_SIZE;

   // sh_text_idx
   gen_shdr(o, SHT_PROGBITS, shdr_names[STEXT], code_off, (int) code_addr,
         code_size, 0, 0, SHF_ALLOC | SHF_EXECINSTR, 4, 0);
   o += SHDR_ENT_SIZE;

   // sh_data_idx
   gen_shdr(o, SHT_PROGBITS, shdr_names[SDATA], rwdata_off, (int) _data,
          dseg_size, 0, 0, SHF_ALLOC | SHF_WRITE, 4, 0);
   o += SHDR_ENT_SIZE;

   int sh_dynstr_idx =
   gen_shdr(o, SHT_STRTAB, shdr_names[SDYNS], dynstr_off, (int) dynstr_addr,
          dynstr_size, 0, 0, SHF_ALLOC, 1, 0);
   o += SHDR_ENT_SIZE;

   int sh_dynsym_idx =
   gen_shdr(o, SHT_DYNSYM, shdr_names[SDYNM], dynsym_off, (int) dynsym_addr,
          dynsym_size, sh_dynstr_idx, 1, SHF_ALLOC, 4, 0x10);
   o += SHDR_ENT_SIZE;

   // sh_dynamic_idx
   gen_shdr(o, SHT_DYNAMIC, shdr_names[SDYNC], pt_dyn_off, (int) pt_dyn,
          pt_dyn_size, sh_dynstr_idx, 0, SHF_ALLOC | SHF_WRITE, 4, 0);
   o += SHDR_ENT_SIZE;

   // sh_interp_idx
   gen_shdr(o, SHT_PROGBITS, shdr_names[SINTP], interp_off, (int) interp,
          interp_str_size, 0, 0, SHF_ALLOC, 1, 0);
   o += SHDR_ENT_SIZE;

   // sh_rel_idx
   gen_shdr(o, SHT_REL, shdr_names[SREL], rel_off, (int) rel_addr,
          rel_size, sh_dynsym_idx, 11, SHF_ALLOC | 0x40, 4, 8);
   o += SHDR_ENT_SIZE;

   // sh_plt_idx
   gen_shdr(o, SHT_PROGBITS, shdr_names[SPLT], plt_off, (int) plt_addr,
          plt_size, 0, 0, SHF_ALLOC | SHF_EXECINSTR, 4, 4);
   o += SHDR_ENT_SIZE;

   // sh_got_idx
   gen_shdr(o, SHT_PROGBITS, shdr_names[SGOT], got_off, (int) got_addr,
          got_size, 0, 0, SHF_ALLOC | SHF_WRITE, 4, 4);
   o += SHDR_ENT_SIZE;

   // Copy .data to a part of (o - buf) where _data located.
   memcpy(dseg, _data, dseg_size);
   write(elf_fd, buf, o - buf);

   free(func_entries);
   free(shdr_names);
   free(freebuf);
   free(freecode);
   free(plt_func_addr);
   free(got_func_slot);
   return 0;
}

enum { _O_CREAT = 64, _O_WRONLY = 1 };

int main(int argc, char **argv)
{
   int *freed_ast, *ast;
   int elf_fd;
   int fd, i;
   int poolsz = 1024 * 1024; // arbitrary size

   if (!(sym = malloc(poolsz)))
      die("could not allocate symbol area");
   memset(sym, 0, poolsz);

   // Register keywords in symbol stack. Must match the sequence of enum
   p = "enum char int float struct union sizeof return goto break continue "
       "if do while for switch case default else __clear_cache sqrtf void main";

   // call "next" to create symbol table entry.
   // store the keyword's token type in the symbol table entry's "tk" field.
   for (i = Enum; i <= Else; ++i) {
      next(); id->tk = i; id->class = Keyword; // add keywords to symbol table
   }

   // add __clear_cache to symbol table
   next(); id->class = ClearCache; id->type = INT; id->val = CLCA;
   next(); id->class = Sqrt; id->type = FLOAT; id->val = SQRT; id->etype = 273;

   next(); id->tk = Char; id->class = Keyword; // handle void type
   next();
   struct ident_s *idmain = id; id->class = Main; // keep track of main

   if (!(freedata = _data = data = malloc(poolsz)))
      printf("could not allocat data area");
   memset(data, 0, poolsz);
   if (!(tsize = malloc(PTR * sizeof(int))))
      die("could not allocate tsize area");
   memset(tsize,   0, PTR * sizeof(int)); // not strictly necessary
   if (!(freed_ast = ast = malloc(poolsz)))
      die("could not allocate abstract syntax tree area");
   memset(ast, 0, poolsz); // not strictly necessary
   ast = (int *) ((int) ast + poolsz); // AST is built as a stack
   n = ast;

   // add primitive types
   tsize[tnew++] = sizeof(char);
   tsize[tnew++] = sizeof(int);
   tsize[tnew++] = sizeof(float);

   --argc; ++argv;
   while (argc > 0 && **argv == '-') {
      if ((*argv)[1] == 's') {
         src = ((*argv)[2] == 'i') ? 2 : 1; --argc; ++argv;
      }
      else if (!strcmp(*argv, "-fsigned-char")) {
         signed_char = 1; --argc; ++argv;
      }
      else if ((*argv)[1] == 'O' && (*argv)[2] == 'p') {
         peephole = 1; --argc; ++argv;
      }
      else if ((*argv)[1] == 'o') {
         elf = 1; --argc; ++argv;
         if (argc < 1) die("no output file argument");
         if ((elf_fd = open(*argv, _O_CREAT | _O_WRONLY, 0775)) < 0) {
            printf("could not open(%s)\n", *argv); return -1;
         }
         --argc; ++argv;
      }
      else if ((*argv)[1] == 'D') {
         p = &(*argv)[2]; next();
         if (tk != Id) fatal("-D bad identifier");
         struct ident_s *dd = id; next(); i = 0;
         if (tk == Assign) {
            next();
            expr(Cond);
            if (*n != Num) fatal("bad -D initializer");
            i = n[1]; n += 2;
         }
         dd->class = Num; dd->type = INT; dd->val = i;
         --argc; ++argv;
      }
      else {
         argc = 0; // bad compiler option. Force exit.
      }
   }
   if (argc < 1)
      die("usage: mc [-s] [-Op] [-fsigned-char] [-o object] file");

   if ((fd = open(*argv, 0)) < 0) {
      printf("could not open(%s)\n", *argv); return -1;
   }

   if (!(text = le = e = malloc(poolsz)))
      die("could not allocate text area");
   if (!(members = malloc(PTR * sizeof(struct member_s *))))
      die("could not malloc() members area");
   if (!(ef_cache = malloc(PTR * sizeof(struct ef_s *))))
      die("could not malloc() external function cache");

   memset(e, 0, poolsz);

   memset(members, 0, PTR * sizeof(struct member_s *));

   if (elf) elf32_init(poolsz); // call before source code parsing

   if (!(freep = lp = p = malloc(poolsz)))
      die("could not allocate source area");
   if ((i = read(fd, p, poolsz - 1)) <= 0)
      die("unable to read from source file");
   p[i] = 0;
   close(fd);

   // real C parser begins here
   // parse the program
   line = 1;
   pplevt = -1;
   next();
   while (tk) {
      stmt(Glo);
      next();
   }

   int ret = elf ? elf32(poolsz, (int *) idmain->val, elf_fd) :
                   jit(poolsz,   (int *) idmain->val, argc, argv);
   free(freep);
   free(freed_ast);
   free(tsize);
   free(freedata);
   free(sym);
   free(text);

   return ret;
}
