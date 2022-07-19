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

#define SMALL_TBL_SZ 256

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
                    // bit 0:1 - tensor rank, eg a[4][4][4]
                    // 0=scalar, 1=1d, 2=2d, 3=3d
                    //   1d etype -- bit 0:30)
                    //   2d etype -- bit 0:15,16:30 [32768,65536]
                    //   3d etype -- bit 0:10,11:20,21:30 [1024,1024,2048]
                    // bit 2:9 - type
                    // bit 10:11 - ptr level
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
int oline, osize;   // for optimization suggestion

// identifier
#define MAX_IR 256
struct ident_s {
   int tk;         // type-id or keyword
   int hash;
   char *name;     // name of this identifier
   int pad;        // pad to multiple of 8 bytes
   /* fields starting with 'h' were designed to save and restore
    * the global class/type/val in order to handle the case if a
    * function declares a local with the same name as a global.
    */
   int class, hclass; // FUNC, GLO (global var), LOC (local var), Syscall
   int type, htype;   // data type such as char and int
   int val, hval;
   int etype, hetype; // extended type info. different meaning for funcs.
} *id,  // currently parsed identifier
  *sym, // symbol table (simple list of identifiers)
  *oid, // for array optimization suggestion
  *ir_var[MAX_IR]; // IR information for local vars and parameters
int ir_count;

// (library) external functions
struct ef_s {
   char *name;
   int addr;
} **ef_cache;
int ef_count;

struct member_s {
   struct member_s *next;
   struct ident_s *id;
   int offset;
   int type;
   int etype;
   int pad;
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
   CastF, Inc, Dec, Dot, Arrow, Bracket, // operator: ++, --, ., ->, [
   Phf // inform peephole optimizer a function call is beginning
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

   VENT, /* 49 Needed fo Varargs ABI, which requires 8-byte stack align */
   VLEV, /* 50 */

   PHD,  /* 51 PeepHole Disable next assembly instruction in optimizer */
   PHF,  /* 52 Inform peephole optimizer a function call is beginning */
   PHR0, /* 53 Inform PeepHole optimizer that R0 holds a return value */

   INVALID
};

// types -- 4 scalar types, 1020 aggregate types, 4 tensor ranks, 8 ptr levels
// bits 0-1 = tensor rank, 2-11 = type id, 12-14 = ptr level
// 4 type ids are scalars: 0 = char/void, 1 = int, 2 = float, 3 = reserved
enum { CHAR = 0, INT = 4, FLOAT = 8, ATOM_TYPE = 11,
       PTR = 0x1000, PTR2 = 0x2000 };

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

char fatal(char *msg)
{
   printf("%d: %.*s\n", line, p - lp, lp);
   printf("%d: %s\n", line, msg); exit(-1);
}

void ef_add(char *name, int addr) // add external function
{
   ef_cache[ef_count] = (struct ef_s *) malloc(sizeof(struct ef_s)) ;
   ef_cache[ef_count]->name = (char *) malloc(strlen(name)+1);
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
         void *divmod_handle = (void *) dlopen("libgcc_s.so.1", 1);
         if (!divmod_handle) fatal("failed to open libgcc_s.so.1");
         dladdr = (int) dlsym(divmod_handle, name);
         if (!dladdr) // fatal("bad function call");
         {
            void *libm_handle = (void *) dlopen("libm.so.6", 1);
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
         if (src) { printf("%d: %.*s", line, p - lp, lp); }
         lp = p; ++line;
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

int popcount32(int i)
{
   i = i - ((i >> 1) & 0x55555555); // add pairs of bits
   i = (i & 0x33333333) + ((i >> 2) & 0x33333333); // quads
   i = (i + (i >> 4)) & 0x0F0F0F0F; // groups of 8
   return (i * 0x01010101) >> 24; // horizontal sum of bytes
}

// verify binary operations are legal
void typecheck(int op, int tl, int tr)
{
   int pt = 0, it = 0, st = 0;
   if (tl >= PTR)  pt += 2; // is pointer?
   if (tr >= PTR)  pt += 1;

   if (tl < FLOAT) it += 2; // is int?
   if (tr < FLOAT) it += 1;

   if (tl > ATOM_TYPE && tl < PTR) st += 2; // is struct/union?
   if (tr > ATOM_TYPE && tr < PTR) st += 1;

   if ((tl ^ tr) & (PTR | PTR2)) { // operation on different pointer levels
      if (op == Add && pt != 3 && (it & ~pt)) ; // ptr + int or int + ptr ok
      else if (op == Sub && pt == 2 && it == 1) ; // ptr - int ok
      else if (op == Assign && pt == 2 && *n == Num && n[1] == 0) ; // ok
      else if (op >= Eq && op <= Le && *n == Num && n[1] == 0) ; // ok
      else fatal("bad pointer arithmetic");
   }
   else if (pt == 3 && op != Assign && op != Sub &&
            (op < Eq || op > Le)) // pointers to same type
      fatal("bad pointer arithmetic");

   if (pt == 0 && op != Assign && (it == 1 || it == 2))
      fatal("cast operation needed");

   if (pt == 0 && st != 0)
      fatal("illegal operation with dereferenced struct");
}

void bitopcheck(int tl, int tr)
{
   if (tl >= FLOAT || tr >= FLOAT)
      fatal("bit operation on non-int types");
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
   int t, tc, tt, nf, *b, sz, *c;
   int otk, memsub = 0;
   union conv *c1, *c2;
   struct ident_s *d;
   struct member_s *m;

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
         t = 0; b = c = 0; tt = 0; nf = 0; // argument count
         if (peephole && d->class != Sqrt) { *--n = Phf; c = n; }
         while (tk != ')') {
            expr(Assign);
            if (c != 0) { *--n = (int) c; *--n = '{'; c = 0; } // peephole
            *--n = (int) b; b = n; ++t;
            tt = tt * 2; if (ty == FLOAT) { ++nf; ++tt; }
            if (tk == ',') {
               next();
               if (tk == ')') fatal("unexpected comma in function call");
            } else if (tk != ')') fatal("missing comma in function call");
         }
         if (t > 22) fatal("maximum of 22 function parameters");
         tt = (tt << 10) + (nf << 5) + t; // func etype not like other etype
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
         if ((d->type & 3) && d->class != Par) { // push reference address
            ty = d->type & ~3;
         }
         else {
            *--n = (ty = d->type & ~3); *--n = Load;
         }
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
      if (tk != '(') fatal("open parenthesis expected in sizeof");
      next(); d = 0;
      if (tk == Id) {
         d = id; ty = d->type; next();
      }
      else {
         ty = INT; // Enum
         switch (tk) {
         case Char:
         case Int:
         case Float:
            ty = (tk - Char) << 2; next(); break;
         case Struct:
         case Union:
            next();
            if (tk != Id || id->type <= ATOM_TYPE || id->type >= PTR)
               fatal("bad struct/union type");
            ty = id->type; next(); break;
         }
         // multi-level pointers, plus `PTR` for each level
         while (tk == Mul) { next(); ty += PTR; }
      }
      if (tk != ')') fatal("close parenthesis expected in sizeof");
      next();
      *--n = (ty & 3) ?
             (((ty - PTR) >= PTR) ? sizeof(int) : tsize[(ty - PTR) >> 2]) :
             ((ty >= PTR) ? sizeof(int) : tsize[ty >> 2]); *--n = Num;
      // just one dimension supported at the moment
      if (d != 0 && (ty & 3)) n[1] *= (id->etype + 1);
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
            t = (tk - Char) << 2; next(); break;
         default:
            next();
            if (tk != Id || id->type <= ATOM_TYPE || id->type >= PTR)
               fatal("bad struct/union type");
            t = id->type; next(); break;
         }
         // t: pointer
         while (tk == Mul) { next(); t += PTR; }
         if (tk != ')') fatal("bad cast");
         next();
         expr(Inc); // cast has precedence as Inc(++)
         if (t != ty && (t == FLOAT || ty == FLOAT)) {
            if (t == FLOAT && ty < FLOAT) { // float : int
               if (*n == Num) { *n = NumF; c1 = &n[1]; c1->f = (float) c1->i; }
               else { b = n; *--n = ITOF; *--n = (int) b; *--n = CastF; }
            }
            else if (t < FLOAT && ty == FLOAT) { // int : float
               if (*n == NumF) { *n = Num; c1 = &n[1]; c1->i = (int) c1->f; }
               else { b = n; *--n = FTOI; *--n = (int) b; *--n = CastF; }
            }
            else fatal("explicit cast required");
         }
         ty = t;
      } else {
         expr(Assign);
         while (tk == ',') {
            next(); b = n;  expr(Assign); *--n = (int) b; *--n = '{';
         }
         if (tk != ')') fatal("close parenthesis expected");
         next();
      }
      break;
   case Mul: // "*", dereferencing the pointer operation
      next();
      expr(Inc); // dereference has the same precedence as Inc(++)
      if (ty < PTR) fatal("bad dereference");
      ty -= PTR; *--n = ty; *--n = Load;
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
      if (ty > ATOM_TYPE && ty < PTR) fatal("!(struct/union) is meaningless");
      if (*n == Num) n[1] = !n[1];
      else { *--n = 0; *--n = Num; --n; *n = (int) (n + 3); *--n = Eq; }
      ty = INT;
      break;
   case '~': // "~x" is equivalent to "x ^ -1"
      next(); expr(Inc); if (ty > ATOM_TYPE) fatal("~ptr is illegal");
      if (*n == Num) n[1] = ~n[1];
      else { *--n = -1; *--n = Num; --n; *n = (int) (n + 3); *--n = Xor; }
      ty = INT;
      break;
   case Add:
      next(); expr(Inc); if (ty > ATOM_TYPE) fatal("unary '+' illegal on ptr");
      break;
   case Sub:
      next(); expr(Inc); if (ty > ATOM_TYPE) fatal("unary '-' illegal on ptr");
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
   case Inc:
   case Dec: // processing ++x and --x. x-- and x++ is handled later
      t = tk; next(); expr(Inc);
      if (ty == FLOAT) fatal("no ++/-- on float");
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
         if (t & 3) fatal("Cannot assign to array type lvalue");
         // the left part is processed by the variable part of `tk=ID`
         // and pushes the address
         if (*n != Load) fatal("bad lvalue in assignment");
         // get the value of the right part `expr` as the result of `a=expr`
         n += 2; b = n; next(); expr(Assign); typecheck(Assign, t, ty);
         *--n = (int) b; *--n = (ty << 16) | t; *--n = Assign; ty = t;
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
         if (t & 3) fatal("Cannot assign to array type lvalue");
         if (*n != Load) fatal("bad lvalue in assignment");
         otk = tk;
         n += 2; b = n; *--n=';'; *--n = t; *--n = Load;
         sz = (t >= PTR2) ? sizeof(int) :
                            ((t >= PTR) ? tsize[(t - PTR) >> 2] : 1);
         next(); c = n; expr(otk);
         if (*n == Num) n[1] *= sz;
         *--n = (int) c;
         if (otk < ShlAssign) {
            *--n = Or + (otk - OrAssign);
         } else {
            *--n = Shl + (otk - ShlAssign);
            // Compound-op bypasses literal const optimizations
            if (ty != FLOAT) {
               if (otk == DivAssign) ef_getidx("__aeabi_idiv");
               if (otk == ModAssign) ef_getidx("__aeabi_idivmod");
            }
         }
         if (t == FLOAT && (otk >= AddAssign && otk <= DivAssign))
            *n += 5;
         typecheck(*n, t, ty);
         *--n = (int) b; *--n = (ty << 16) | t; *--n = Assign; ty = t;
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
         next(); expr(Xor); bitopcheck(t, ty);
         if (*n == Num && *b == Num) n[1] = b[1] | n[1];
         else { *--n = (int) b; *--n = Or; }
         ty = INT;
         break;
      case Xor:
         next(); expr(And); bitopcheck(t, ty);
         if (*n == Num && *b == Num) n[1] = b[1] ^ n[1];
         else { *--n = (int) b; *--n = Xor; }
         ty = INT;
         break;
      case And:
         next(); expr(Eq); bitopcheck(t, ty);
         if (*n == Num && *b == Num) n[1] = b[1] & n[1];
         else { *--n = (int) b; *--n = And; }
         ty = INT;
         break;
      case Eq:
         next(); expr(Ge); typecheck(Eq, t, ty);
         if (ty == FLOAT) {
            if (*n == NumF && *b == NumF) {
               c1 = &n[1]; c2 = &b[1]; c1->i = (c2->f == c1->f); *n = Num;
            }
            else { *--n = (int) b; *--n = EqF; }
         } else {
            if (*n == Num && *b == Num) n[1] = b[1] == n[1];
            else { *--n = (int) b; *--n = Eq; }
         }
         ty = INT;
         break;
      case Ne:
         next(); expr(Ge); typecheck(Ne, t, ty);
         if (ty == FLOAT) {
            if (*n == NumF && *b == NumF) {
               c1 = &n[1]; c2 = &b[1]; c1->i = (c2->f != c1->f); *n = Num;
            }
            else { *--n = (int) b; *--n = NeF; }
         } else {
            if (*n == Num && *b == Num) n[1] = b[1] != n[1];
            else { *--n = (int) b; *--n = Ne; }
         }
         ty = INT;
         break;
      case Ge:
         next(); expr(Shl); typecheck(Ge, t, ty);
         if (ty == FLOAT) {
            if (*n == NumF && *b == NumF) {
               c1 = &n[1]; c2 = &b[1]; c1->i = (c2->f >= c1->f); *n = Num;
            }
            else { *--n = (int) b; *--n = GeF; }
         } else {
            if (*n == Num && *b == Num) n[1] = b[1] >= n[1];
            else { *--n = (int) b; *--n = Ge; }
         }
         ty = INT;
         break;
      case Lt:
         next(); expr(Shl); typecheck(Lt, t, ty);
         if (ty == FLOAT) {
            if (*n == NumF && *b == NumF) {
               c1 = &n[1]; c2 = &b[1]; c1->i = (c2->f < c1->f); *n = Num;
            }
            else { *--n = (int) b; *--n = LtF; }
         } else {
            if (*n == Num && *b == Num) n[1] = b[1] < n[1];
            else { *--n = (int) b; *--n = Lt; }
         }
         ty = INT;
         break;
      case Gt:
         next(); expr(Shl); typecheck(Gt, t, ty);
         if (ty == FLOAT) {
            if (*n == NumF && *b == NumF) {
               c1 = &n[1]; c2 = &b[1]; c1->i = (c2->f > c1->f); *n = Num;
            }
            else { *--n = (int) b; *--n = GtF; }
         } else {
            if (*n == Num && *b == Num) n[1] = b[1] > n[1];
            else { *--n = (int) b; *--n = Gt; }
         }
         ty = INT;
         break;
      case Le:
         next(); expr(Shl); typecheck(Le, t, ty);
         if (ty == FLOAT) {
            if (*n == NumF && *b == NumF) {
               c1 = &n[1]; c2 = &b[1]; c1->i = (c2->f <= c1->f); *n = Num;
            }
            else { *--n = (int) b; *--n = LeF; }
         } else {
            if (*n == Num && *b == Num) n[1] = b[1] <= n[1];
            else { *--n = (int) b; *--n = Le; }
         }
         ty = INT;
         break;
      case Shl:
         next(); expr(Add); bitopcheck(t, ty);
         if (*n == Num && *b == Num) {
            if (n[1] < 0) n[1] = b[1] >> -n[1];
            else n[1] = b[1] << n[1];
         } else { *--n = (int) b; *--n = Shl; }
         ty = INT;
         break;
      case Shr:
         next(); expr(Add); bitopcheck(t, ty);
         if (*n == Num && *b == Num) {
            if (n[1] < 0) n[1] = b[1] << -n[1];
            else n[1] = b[1] >> n[1];
         } else { *--n = (int) b; *--n = Shr; }
         ty = INT;
         break;
      case Add:
         next(); expr(Mul); typecheck(Add, t, ty);
         if (ty == FLOAT) {
            if (*n == NumF && *b == NumF) { 
               c1 = &n[1]; c2 = &b[1]; c1->f = c1->f + c2->f;
            }
            else { *--n = (int) b; *--n = AddF; }
         }
         else { // both terms are either int or "int *"
            tc = ((t | ty) & (PTR | PTR2)) ? (t >= PTR) : (t >= ty);
            c = n; if (tc) ty = t;
            sz = (ty >= PTR2) ? sizeof(int) :
                 ((ty >= PTR) ? tsize[(ty - PTR) >> 2] : 1);
            if (*n == Num && tc) { n[1] *= sz; sz = 1; }
            else if (*b == Num && !tc) { b[1] *= sz; sz = 1; }
            if (*n == Num && *b == Num) n[1] += b[1];
            else if (sz != 1) {
               *--n = sz; *--n = Num;
               *--n = (int) (tc ? c : b); *--n = Mul;
               *--n = (int) (tc ? b : c); *--n = Add;
            }
            else { *--n = (int) b; *--n = Add; }
         }
         break;
      case Sub:
         next(); expr(Mul); typecheck(Sub, t, ty);
         if (ty == FLOAT) {
            if (*n == NumF && *b == NumF) { 
               c1 = &n[1]; c2 = &b[1]; c1->f = c2->f - c1->f;
            }
            else { *--n = (int) b; *--n = SubF; }
         }
         else { // 4 cases: ptr-ptr, ptr-int, int-ptr (err), int-int
            if (t >= PTR) { // left arg is ptr
               sz = (t >= PTR2) ? sizeof(int) : tsize[(t - PTR) >> 2];
               if (ty >= PTR) { // ptr - ptr
                  if (*n == Num && *b == Num) n[1] = (b[1] - n[1]) / sz;
                  else {
                     *--n = (int) b; *--n = Sub;
                     if (sz > 1) {
                        if ((sz & (sz - 1)) == 0) { // 2^n
                           *--n = popcount32(sz - 1); *--n = Num;
                           --n; *n = (int) (n + 3); *--n = Shr;
                        }
                        else {
                           *--n = sz; *--n = Num; --n; *n = (int) (n + 3);
                           *--n = Div; ef_getidx("__aeabi_idiv");
                        }
                     }
                  }
                  ty = INT;
               }
               else { // ptr - int
                  if (*n == Num) {
                     n[1] *= sz;
                     if (*b == Num) n[1] = b[1] - n[1];
                     else { *--n = (int) b; *--n = Sub; }
                  }
                  else {
                     if (sz > 1) {
                        if ((sz & (sz - 1)) == 0) { // 2^n
                           *--n = popcount32(sz - 1); *--n = Num;
                           --n; *n = (int) (n + 3); *--n = Shl;
                        }
                        else {
                           *--n = sz; *--n = Num;
                           --n; *n = (int) (n + 3); *--n = Mul;
                        }
                     }
                     *--n = (int) b; *--n = Sub;
                  }
                  ty = t;
               }
            }
            else { // int - int
               if (*n == Num && *b == Num) n[1] = b[1] - n[1];
               else { *--n = (int) b; *--n = Sub; }
               ty = INT;
            }
         }
         break;
      case Mul:
         next(); expr(Inc); typecheck(Mul, t, ty);
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
                  n[2] = popcount32(n[2] - 1); *--n = Shl; // 2^n
               }
               else *--n = Mul;
            }
            ty = INT;
         }
         break;
      case Inc:
      case Dec:
         if (ty & 3) fatal("can't inc/dec an array variable");
         if (ty == FLOAT) fatal("no ++/-- on float");
         sz = (ty >= PTR2) ? sizeof(int) :
              ((ty >= PTR) ? tsize[(ty - PTR) >> 2] : 1);
         if (*n != Load) fatal("bad lvalue in post-increment");
         *n = tk;
         *--n = sz; *--n = Num;
         *--n = (int) b; *--n = (tk == Inc) ? Sub : Add;
         next();
         break;
      case Div:
         next(); expr(Inc); typecheck(Div, t, ty);
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
                  n[2] = popcount32(n[2] - 1); *--n = Shr; // 2^n
               } else {
                  *--n = Div;
                  ef_getidx("__aeabi_idiv");
               }
            }
            ty = INT;
         }
         break;
      case Mod:
         next(); expr(Inc); typecheck(Mod, t, ty);
         if (ty == FLOAT) fatal("use fmodf() for float modulo");
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
         t += PTR;
         if (n[0] == Load && n[1] > ATOM_TYPE && n[1] < PTR) n += 2; // struct
      case Arrow:
         if (t <= PTR+ATOM_TYPE || t >= PTR2) fatal("structure expected");
         next();
         if (tk != Id) fatal("structure member expected");
         m = members[(t - PTR) >> 2]; while (m && m->id != id) m = m->next;
         if (!m) fatal("structure member not found");
         if (m->offset) {
            *--n = m->offset; *--n = Num; --n; *n = (int) (n + 3);
            *--n = Add;
         }
         ty = m->type; next();
         if (!(ty & 3)) {
            *--n = (ty >= PTR) ? INT : ty; *--n = Load; break;
         }
         memsub = 1; int dim = ty & 3, ee = m->etype; b = n; t = ty & ~3;
      case Bracket:
         if (t < PTR) fatal("pointer type expected");
         if (memsub == 0) { dim = id->type & 3, ee = id->etype; }
         int sum = 0, ii = dim - 1, *f = 0;
         int doload = 1; memsub = 0;
         sz = ((t = t - PTR) >= PTR) ? sizeof(int) : tsize[t >> 2];
         do {
            if (dim && tk != Bracket) { // ptr midway for partial subscripting
              t += PTR*(ii+1); doload = 0; break;
            }
            next(); expr(Assign); if (ty >= FLOAT) fatal("non-int array index");
            if (tk != ']') fatal("close bracket expected");
            c = n; next();
            if (dim) {
               int factor = ((ii == 2) ? (((ee>>11) & 0x3ff) + 1) : 1);
               factor *= ((dim == 3 && ii >= 1) ? ((ee & 0x7ff) + 1) :
                        ((dim == 2 && ii == 1) ? ((ee & 0xffff) + 1) : 1)) ;
               if (*n == Num) {
                  // elision with struct offset for efficiency
                  if (*b == Add && b[2] == Num) b[3] += factor * n[1] * sz;
                  else sum += factor * n[1];
                  n += 2; // delete the subscript constant
               }
               else {
                  // generate code to add a term
                  if (factor > 1) {
                     *--n = factor; *--n = Num;
                     *--n = (int) c; *--n = Mul;
                  }
                  if (f) { *--n = (int) f; *--n = Add; }
                  f = n;
               }
            }
         } while (--ii >= 0) ;
         if (dim) {
            if (sum != 0) {
               if (f) {
                  *--n = sum; *--n = Num;
                  *--n = (int) f; *--n = Add;
               }
               else { sum *= sz; sz = 1; *--n = sum; *--n = Num; }
            }
            else if (!f) goto add_simple;
         }
         if (sz > 1) {
            if (*n == Num) n[1] *= sz;
            else {
               *--n = sz; *--n = Num; --n; *n = (int) (n + 3); *--n = Mul;
            }
         }
         if (*n == Num && *b == Num) n[1] += b[1];
         else { *--n = (int) b; *--n = Add; }
add_simple:
         if (doload) { *--n = ((ty = t) >= PTR) ? INT : ty; *--n = Load; }
         break;
      default:
         printf("%d: compiler error tk=%d\n", line, tk); exit(-1);
      }
   }
}

void init_array(struct ident_s *tn, int extent[], int dim)
{
   int i, cursor, match, coff = 0, off, empty, *vi;
   int inc[3];

   inc[0] = extent[dim-1];
   for (i = 1; i < dim; ++i) inc[i] = inc[i-1] * extent[dim-(i+1)];

   // Global is preferred to local.
   // Either suggest global or automatically move to global scope.
   if (tn->class != Glo) fatal("only global array initialization supported");

   switch (tn->type & ~3) {
      case (CHAR  | PTR2) : fatal("Use extra dim of MAXCHAR length instead");
      case (CHAR  | PTR ) : match = CHAR + PTR; coff = 1; break; // strings
      case (INT   | PTR ) : match = INT; break;
      case (FLOAT | PTR ) : match = FLOAT; break;
      default:
         fatal("array-init must be literal ints, floats, or strings");
   }

   vi = (int *) tn->val; i = 0; cursor = (dim - coff);
   do {
      if (tk == '{') {
         next();
         if (cursor) --cursor;
         else fatal("overly nested initializer");
         empty = 1; continue;
      }
      else if (tk == '}') {
         next();
         // skip remainder elements on this level (or set 0 if cmdline opt)
         if ((off = i % inc[cursor+coff]) || empty)
            i += (inc[cursor+coff] - off);
         if (++cursor == dim - coff) break;
      }
      else {
         expr(Cond);
         if (*n != Num && *n != NumF) fatal("non-literal initializer");

         if (ty == CHAR + PTR) {
            if (match == CHAR + PTR) {
               off = strlen((char *) n[1]) + 1;
               if (off > inc[0]) {
                  off = inc[0];
                  printf("%d: string '%s' truncated to %d chars\n",
                         line, (char *) n[1], off);
               }
               memcpy((char *)vi + i, (char *) n[1], off);
               i += inc[0];
            }
            else fatal("can't assign string to scalar");
         }
         else if (ty == match) { vi[i++] = n[1]; }
         else if (ty == INT) {
            if (match == CHAR + PTR) { *((char *) vi + i) = n[1]; i += inc[0];}
            else { *((float *)(n+1)) = (float) n[1]; vi[i++] = n[1]; }
         }
         else if (ty == FLOAT) {
            if (match == INT) { vi[i++] = (int) *((float *)(n+1)); }
            else fatal("illegal char/string initializer");
         }
         n += 2; // clean up AST
         empty = 0;
      }
      if (tk == ',') next();
   } while(1);
}


// AST parsing for IR generatiion
// With a modular code generator, new targets can be easily supported such as
// native Arm machine code.
void gen(int *n)
{
   int i = *n, j, k, l, isPrtf;
   int *a, *b, *c, *d, *t;
   struct ident_s *label;

   switch (i) {
   case Num: *++e = IMM; *++e = n[1]; break;   // int value
   case NumF: *++e = IMMF; *++e = n[1]; break; // float value
   case Load:
      gen(n + 2); // load the value
      if (n[1] > ATOM_TYPE && n[1] < PTR) // unreachable?
         fatal("struct copies not yet supported");
      *++e = (n[1] >= PTR) ? LI : LC + (n[1] >> 2);
      break;
   case Loc: *++e = LEA; *++e = n[1]; break;       // get address of variable
   case '{': gen((int *) n[1]); gen(n + 2); break; // parse AST expr or stmt
   case Assign: // assign the value to variables
      gen((int *) n[2]); *++e = PSH; gen(n + 3); l = n[1] & 0xffff;
      // Add SC/SI instruction to save value in register to variable address
      // held on stack.
      if (l > ATOM_TYPE && l < PTR) fatal("struct assign not yet supported");
      if ((n[1] >> 16) == FLOAT && l == INT) *++e = FTOI;
      else if ((n[1] >> 16) == INT && l == FLOAT) *++e = ITOF;
      *++e = (l >= PTR) ? SI : SC + (l >> 2);
      break;
   case Inc: // increment or decrement variables
   case Dec:
      gen(n + 2);
      *++e = PSH; *++e = (n[1] == CHAR) ? LC : LI; *++e = PSH;
      *++e = IMM; *++e = (n[1] >= PTR2) ? sizeof(int) :
                         ((n[1] >= PTR) ? tsize[(n[1] - PTR) >> 2] : 1);
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
         if (i == Syscall) {
            isPrtf = (n[4] & 0x3e0) && !strcmp(ef_cache[n[2]]->name,"printf");
            if (isPrtf) {
               *++e = PHD; *++e = IMM; *++e = (k & 1)*4; *++e = VENT;
            }
         }
         l = (i != ClearCache) ? (n[4] >> 10) : 0;
         a = (int *) malloc(sizeof(int) * k);
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
      if (k && i == Syscall && isPrtf) *++e = VLEV;
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
      if (*(e - 1) != IMM) fatal("case label not a numeric literal");
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
   case Phf: *++e = PHF; break;
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

void loc_array_decl(int ct, int extent[3], int *dims, int *et, int *size)
{
   int ii = ii; // keep this to disable frame optimization for now.
   *dims = 0;
   do {
      next();
      if (*dims == 0 && ct == Par && tk == ']') {
         extent[*dims] = 1; next();
      }
      else {
         expr(Cond);
         if (*n != Num) fatal("non-const array size");
         if (n[1] <= 0) fatal("non-positive array dimension");
         if (tk != ']') fatal("missing ]");
         next(); extent[*dims] = n[1]; *size *= n[1]; n += 2;
      }
      ++*dims;
   } while (tk == Bracket && *dims < 3);
   if (tk == Bracket) fatal("three subscript max on decl");
   switch(*dims) {
   case 1:
      *et = (extent[0]-1); break;
   case 2:
      *et = ((extent[0]-1) << 16) + (extent[1]-1);
      if (extent[0] > 32768 || extent[1] > 65536)
         fatal("max bounds [32768][65536]");
      break;
   case 3:
      *et = ((extent[0]-1) << 21) + ((extent[1]-1) << 11) + (extent[2]-1);
      if (extent[0] > 1024 || extent[1] > 1024 || extent[2] > 2048)
         fatal("max bounds [1024][1024][2048]");
      break;
   }
}

// statement parsing (syntax analysis, except for declarations)
void stmt(int ctx)
{
   struct ident_s *dd;
   int *a, *b, *c, *d;
   int i, j, nf, atk, sz;
   int nd[3];
   int bt;

   if (ctx == Glo && (tk < Enum || tk > Union))
      fatal("syntax: statement used outside function");

   switch (tk) {
   case Enum:
      next();
      // If current token is not "{", it means having enum type name.
      // Skip the enum type name.
      if (tk == Id) next();
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
         ir_var[ir_count++] = id;
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
         bt = (tk - Char) << 2; next(); break;
      case Struct:
      case Union:
         atk = tk; next();
         if (tk == Id) {
            if (!id->type) id->type = tnew++ << 2;
            bt = id->type;
            next();
         } else {
            bt = tnew++ << 2;
         }
         if (tk == '{') {
            next();
            if (members[bt >> 2]) fatal("duplicate structure definition");
            tsize[bt >> 2] = 0; // for unions
            i = 0;
            while (tk != '}') {
               int mbt = INT; // Enum
               switch (tk) {
               case Char:
               case Int:
               case Float:
                  mbt = (tk - Char) << 2; next(); break;
               case Struct:
               case Union:
                  next();
                  if (tk != Id || id->type <= ATOM_TYPE || id->type >= PTR)
                     fatal("bad struct/union declaration");
                  mbt = id->type;
                  next(); break;
               }
               while (tk != ';') {
                  ty = mbt;
                  // if the beginning of * is a pointer type,
                  // then type plus `PTR` indicates what kind of pointer
                  while (tk == Mul) { next(); ty += PTR; }
                  if (tk != Id) fatal("bad struct member definition");
                  sz = (ty >= PTR) ? sizeof(int) : tsize[ty >> 2];
                  struct member_s *m = (struct member_s *)
                                       malloc(sizeof(struct member_s));
                  m->id = id; m->etype = 0; next();
                  if (tk == Bracket) {
                     j = ty; loc_array_decl(0, nd, &nf, &m->etype, &sz);
                     ty = (j + PTR) | nf;
                  }
                  sz = (sz + 3) & -4;
                  m->offset = i;
                  m->type = ty;
                  m->next = members[bt >> 2];
                  members[bt >> 2] = m;
                  i += sz;
                  if (atk == Union) {
                     if (i > tsize[bt >> 2]) tsize[bt >> 2] = i; i = 0;
                  }
                  if (tk == ',') next();
               }
               next();
            }
            next();
            if (atk != Union) tsize[bt >> 2] = i;
         }
         break;
      }
      /* parse statement such as 'int a, b, c;'
       * "enum" finishes by "tk == ';'", so the code below will be skipped.
       * While current token is not statement end or block end.
       */
      b = 0;
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
         dd = id; dd->type = ty;
         if (tk == '(') { // function
            if (b != 0) fatal("func decl can't be mixed with var decl(s)");
            if (ctx != Glo) fatal("nested function");
            if (ty > ATOM_TYPE && ty < PTR)
               fatal("return type can't be struct");
            if (id->class == Syscall && id->val)
               fatal("forward decl location failed one pass compilation");
            if (id->class == Func &&
               id->val > (int) text && id->val < (int) e)
               fatal("duplicate global definition");
            dd->etype = 0; dd->class = Func; // type is function
            dd->val = (int) (e + 1); // function Pointer? offset/address
            next(); nf = ir_count = ld = 0; // "ld" is parameter's index.
            while (tk != ')') {
               stmt(Par);
               dd->etype = dd->etype * 2;
               if (ty == FLOAT) { ++nf; ++(dd->etype); }
               if (tk == ',') next();
            }
            if (ld > 22) fatal("maximum of 22 function parameters");
            // function etype is not like other etypes
            next(); dd->etype = (dd->etype << 10) + (nf << 5) + ld; // prm info
            if (tk == ';') { dd->val = 0; goto unwind_func; } // fn proto
            if (tk != '{') fatal("bad function definition");
            loc = ++ld;
            next();
            oline = -1; osize = -1; oid = 0; // optimization hint
            // Not declaration and must not be function, analyze inner block.
            // e represents the address which will store pc
            // (ld - loc) indicates memory size to allocate
            *--n = ';';
            while (tk != '}') {
               int *t = n; check_label(&t); stmt(Loc);
               if (t != n) { *--n = (int) t; *--n = '{'; }
            }
            *--n = ld - loc; *--n = Enter;
            if (oid && n[1] >= 64)
               printf("--> %d: move %.*s to global scope for performance.\n",
                      oline, (oid->hash & 0x3f), oid->name);
            cas = 0;
            gen(n);
            if (src) {
               int *base = le;
               printf("%d: %.*s\n", line, p - lp, lp); lp = p;
               while (le < e) {
                  int off = le - base; // Func IR instruction memory offset
                  printf("%04d: %8.4s", off,
                        & "LEA  IMM  IMMF JMP  JSR  BZ   BNZ  ENT  ADJ  LEV  "
                          "PSH  PSHF LC   LI   LF   SC   SI   SF   "
                          "OR   XOR  AND  EQ   NE   GE   LT   GT   LE   "
                          "SHL  SHR  ADD  SUB  MUL  DIV  MOD  "
                          "ADDF SUBF MULF DIVF FTOI ITOF "
                          "EQF  NEF  GEF  LTF  GTF  LEF  SQRT "
                          "SYSC CLCA VENT VLEV PHD  PHF  PHR0" [*++le * 5]);
                  if (*le < ADJ) {
                     struct ident_s *scan;
                     ++le;
                     if (*le > (int) base && *le <= (int) e)
                        printf(" %04d\n", off + ((*le - (int) le) >> 2) + 1);
                     else if (*(le - 1) == LEA && src == 2) {
                        int ii = 0;
                        for (scan = ir_var[ii];  scan; scan = ir_var[++ii])
                           if (loc - scan->val == *le) {
                              printf(" %.*s (%d)\n",
                                     scan->hash & 0x3f, scan->name, *le);
                              break;
                           }
                     }
                     else if ((*le & 0xf0000000) &&
                              (*le > 0 || -*le > 0x1000000)) {
                        for (scan = sym; scan->tk; ++scan)
                           if (scan->val == *le) {
                              printf(" &%.*s", scan->hash & 0x3f, scan->name);
                              if (src == 2) printf(" (0x%08x)", *le);
                              printf("\n");
                              break;
                           }
                        if (!scan->tk) printf(" 0x%08x\n", *le);
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
unwind_func: id = sym;
            if (src) memset(ir_var, 0, sizeof(struct ident_s *)*MAX_IR);
            while (id->tk) { // unwind symbol table locals
               if (id->class == Loc || id->class == Par) {
                  id->class = id->hclass;
                  id->type = id->htype;
                  id->val = id->hval;
                  id->etype = id->hetype;
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
            dd->hclass = dd->class; dd->class = ctx;
            dd->htype = dd->type; dd->type = ty;
            dd->hval = dd->val;
            dd->hetype = dd->etype;
            sz = (ty >= PTR) ? sizeof(int) : tsize[ty >> 2];
            if (tk == Bracket) {
               i = ty; loc_array_decl(ctx, nd, &j, &dd->etype, &sz);
               ty = (i + PTR) | j; dd->type = ty;
            }
            sz = (sz + 3) & -4;
            if (ctx == Loc && sz > osize) {
               osize = sz; oline = line; oid = dd;
            }
            if (ctx == Glo) { dd->val = (int) data; data += sz; }
            else if (ctx == Loc) {
               dd->val = (ld += sz / sizeof(int)); ir_var[ir_count++] = dd;
            }
            else if (ctx == Par) {
               if (ty > ATOM_TYPE && ty < PTR) // local struct decl
                  fatal("struct parameters must be pointers");
               dd->val = ld++; ir_var[ir_count++] = dd;
            }
            if (tk == Assign) {
               next();
               if (ctx == Par) fatal("default arguments not supported");
               if (tk == '{' && (dd->type & 3)) init_array(dd, nd, j);
               else {
                  if (ctx == Loc) {
                     if (b == 0) *--n = ';';
                     b = n; *--n = loc - dd->val; *--n = Loc;
                     a = n; i = ty; expr(Assign); typecheck(Assign, i, ty);
                     *--n = (int)a; *--n = (ty << 16) | i; *--n = Assign;
                     ty = i; *--n = (int) b; *--n = '{';
                  }
                  else { // ctx == Glo
                     i = ty; expr(Cond); typecheck(Assign, i, ty);
                     if (ty == CHAR + PTR && (dd->type & 3) != 1)
                        fatal("use decl char foo[nn] = \"...\";");
                     if ((*n == Num || *n == NumF) && (i == INT || i == FLOAT))
                        *((int *) dd->val) = tkv.i;
                     else if (ty == CHAR + PTR) {
                        i = strlen((char *) tkv.i) + 1;
                        if (i > (dd->etype + 1)) {
                            i = dd->etype + 1;
                            printf("%d: string truncated to width\n", line);
                        }
                        memcpy((char *) dd->val, (char *) tkv.i, i);
                     }
                     else fatal("global assignment must eval to lit expr");
                     n += 2;
                  }
               }
            }
         }
         if (ctx != Par && tk == ',') next();
      }
      return;
   case If:
      next();
      if (tk != '(') fatal("open parenthesis expected");
      next();
      expr(Assign); a = n;
      if (tk != ')') fatal("close parenthesis expected");
      next();
      stmt(ctx);
      b = n;
      if (tk == Else) { next(); stmt(ctx); d = n; } else d = 0;
      *--n = (int)d; *--n = (int) b; *--n = (int) a; *--n = Cond;
      return;
   case While:
      next();
      if (tk != '(') fatal("open parenthesis expected");
      next();
      expr(Assign); b = n; // condition
      if (tk != ')') fatal("close parenthesis expected");
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
      if (tk != '(') fatal("open parenthesis expected");
      next();
      *--n = ';';
      expr(Assign); b = n;
      if (tk != ')') fatal("close parenthesis expected");
      next();
      *--n = (int) b; *--n = (int) a; *--n = DoWhile;
      return;
   case Switch:
      i = 0; j = 0;
      if (cas) j = (int) cas;
      cas = &i;
      next();
      if (tk != '(') fatal("open parenthesis expected");
      next();
      expr(Assign);
      a = n;
      if (tk != ')') fatal("close parenthesis expected");
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
      if (*n != Num) fatal("case label not a numeric literal");
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
      if (tk != '(') fatal("open parenthesis expected");
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
      if (tk != ')') fatal("close parenthesis expected");
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
   int i, ii, jj, kk, tmp, c, ni, nf, sz;
   int *je, *tje;    // current position in emitted native code
   int *immloc, *immlocv, *il, *ill, *ivv;
   int *rMap;

   immloc = il = (int *) malloc(1024 * sizeof(int));
   int *iv = (int *) malloc(1024 * sizeof(int));
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
      if (peephole && i == ENT) {
         // align on 16 byte boundary
         while((int)je & 0x0f) *je++ = 0xe1a00000; // mov r0, r0
         // add space for 16 frame register constants
         for (ii=0; ii<16; ++ii) *je++ = 0xe1a00000; // mov r0, r0
      }
      jitmap[((int) pc++ - (int) text) >> 2] = (int) je;
      switch (i) {
      case LEA:
         tmp = *pc++;
         if (tmp >= 64 || tmp <= -64) {
            if (!imm0) imm0 = je; *il++ = (int) (je++); *iv++ = tmp * 4;
            *je++ = 0xe08b0000;   // add r0, fp, r0
         }
         else if (tmp >= 0)
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
         if (peephole) { // reserve space for frame registers
            for (ii=0; ii<16; ++ii) *je++ = 0xe1a00000; // mov r0, r0
         }
         break;
      case ADJ:
         *je++ = 0xe28dd000 + ( *pc++ & 0x1f) * 4; // add sp, sp, #(tmp * 4)
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
         if ((pc[2] & 0x1f) > 10) die("codegen: no support for 11+ arguments");
         c = pc[2]; ii = c & 0x1f; c >>= 5; nf = c & 0x1f; ni = ii - nf; c >>= 5;
         tmp = ef_getaddr(*pc);  // look up address from ef index

         // Handle ridiculous printf() varargs ABI inline
         int isPrtf = nf && !strcmp(ef_cache[*pc]->name, "printf");
         if (isPrtf) {
            if (ii == 0) die("printf() has no arguments");
            jj = ii - 1; sz = 0; rMap = (int *) malloc(ii*sizeof(int));
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
            if (sz > 4) *je++ = 0xe24dd000 | (sz - 4)*4; // sub sp, sp, #off
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
                     *je++ = 0xeddd0a00 | (jj + (sz - 4));
                     *je++ = 0xeeb70ae0;
                     *je++ = 0xed8d0b00 | (rMap[jj]-4);
                  }
                  else { // int
                     // ldr r0, [sp, #(sz + jj*4)]
                     // str r0, [sp, #rMap[jj]-4]
                     *je++ = 0xe59d0000 | ((sz - 4) + jj)*4;
                     *je++ = 0xe58d0000 | (rMap[jj]*4 - 16);
                  }
               }
            }
            for(; jj < ii; ++jj) { // handle values in regs
               if (c & (1 << jj)) { // float
                  // vldr s"rMap[jj]+1", [sp, #(sz + jj*4)]
                  // fcvtds d"rMap[jj]/2", s"rMap[jj]+1"
                  // vmov r"rMap[jj]", r"rMap[jj]+1", d"rMap[jj]/2"
                  *je++ = 0xeddd0a00 | (rMap[jj] << 11) | (jj + (sz - 4));
                  *je++ = 0xeeb70ae0 | (rMap[jj] << 11) | (rMap[jj] >> 1);
                  if (peephole) *je++ = 0xe1a01001;  // mov r1, r1
                  *je++ = 0xec500b10 | (rMap[jj] << 12) |
                          ((rMap[jj] + 1) << 16) | (rMap[jj] >> 1);
               }
               else { // int
                  if (peephole) *je++ = 0xe1a01001;  // mov r1, r1
                  // ldr r"rMap[jj]", [sp, #(sz + jj*4)]
                  *je++ = 0xe59d0000 | (rMap[jj] << 12) | ((sz - 4) + jj)*4;
               }
            }
            ++pc; // point at ADJ instruction
         }
         else {
            pc = pc + 3; kk = ni; // skip ADJ instruction
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
               *je++ = 0xe92d03f0; // push [r4-r9]
            }
         }
         if (peephole) *je++ = 0xe1a01001;     // mov r1, r1
         *je++ = 0xe28fe000;                   // add lr, pc, #0
         if (!imm0) imm0 = je; *il++ = (int) je++ + 1; *iv++ = tmp;
         if (isPrtf) {
            if (sz > 4) *je++ = 0xe28dd000 | (sz - 4)*4; // add sp, sp, #off
            free(rMap);
         }
         else {
            if (ni > 4) {
               if (peephole) *je++ = 0xe1a01001;  // mov r1, r1
               *je++ = 0xe28dd018;                // add sp, sp, #24
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
      case VENT:
         if (peephole) *je++ = 0xe1a01001;  // mov r1, r1
         *je++ = 0xe08d0000; // add r0, sp, r0
         if (peephole) *je++ = 0xe1a01001;  // mov r1, r1
         *je++ = 0xe3100004; // tst r0, #4   -- lower 2 bits always 0
         if (peephole) *je++ = 0xe1a01001;  // mov r1, r1
         *je++ = 0x152dd004; // pushne {sp}
         break;
      case VLEV: // Note: r0 contains return value
         if (peephole) *je++ = 0xe1a01001;  // mov r1, r1
         *je++ = 0xe59d1000; // ldr r1, [sp]
         if (peephole) *je++ = 0xe1a01001;  // mov r1, r1
         *je++ = 0xe2411004; // sub r1, r1, #4
         if (peephole) *je++ = 0xe1a01001;  // mov r1, r1
         *je++ = 0xe151000d; // cmp r1, sp
         if (peephole) *je++ = 0xe1a01001;  // mov r1, r1
         *je++ = 0x049d1004; // popeq {r1}
         break;
      case PHD:  *je++ = 0xe1a01001; break;      // mov r1, r1
      case PHF:  *je++ = 0xe1a0b00b; break;      // mov fp, fp
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
            if (*tje != 0xe1a01001 &&    // NOP : mov r1, r1
                (*tje & 0xfff00ff0) != 0xe0000090) { // mul
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
   free(iv); free(immloc);
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
   if (!(jitmem = (char *) mmap(0, poolsz,
                                _PROT_EXEC | _PROT_READ | _PROT_WRITE,
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
   int *res = (int *) bsearch(&sym, sym, 1, 1, (void *) _start);
   if (((int *) 0) != res) return 0; return -1; // make compiler happy
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
   freebuf = (char *) malloc(poolsz);
   char *o = (char *) (((int) freebuf + PAGE_SIZE - 1)  & -PAGE_SIZE);
   /* We must assign the plt_func_addr[x] a non-zero value, and also,
    * plt_func_addr[i] and plt_func_addr[i-1] has an offset of 16
    * (4 instruction * 4 bytes), so the first codegen and second codegen
    * have consistent code_size. Dummy address at this point.
    */
   plt_func_addr = (char **) malloc(sizeof(char *) * SMALL_TBL_SZ);
   for (i = 0; i < SMALL_TBL_SZ; ++i)
      plt_func_addr[i] = o + i * 16;

   ef_getidx("__libc_start_main"); // slot 0 of external func cache
}

int elf32(int poolsz, int *main, int elf_fd)
{
   int i;
   char *freecode;
   char *code = freecode = (char *) malloc(poolsz);
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
   // e_phentsize & e_phnum
   *o++ = PHDR_ENT_SIZE; *o++ = 0; *o++ = PHDR_NUM; *o++ = 0;
   // e_shentsize & e_shnum
   *o++ = SHDR_ENT_SIZE; *o++ = 0; *o++ = SHDR_NUM; *o++ = 0;
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

   int *shdr_names = (int *) malloc(sizeof(int) * SHDR_NUM);
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

   int *func_entries = (int *) malloc(sizeof(int) * ef_count);
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
   char **got_func_slot = (char **) malloc(sizeof(char *) * ef_count);
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

   if (!(sym = (struct ident_s *) malloc(poolsz)))
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
   next(); id->class = Sqrt; id->type = FLOAT; id->val = SQRT; id->etype = 1057;

   next(); id->tk = Char; id->class = Keyword; // handle void type
   next();
   struct ident_s *idmain = id; id->class = Main; // keep track of main

   if (!(freedata = _data = data = (char *) malloc(poolsz)))
      printf("could not allocat data area");
   memset(data, 0, poolsz);
   if (!(tsize = (int *) malloc(SMALL_TBL_SZ * sizeof(int))))
      die("could not allocate tsize area");
   memset(tsize,   0, SMALL_TBL_SZ * sizeof(int)); // not strictly necessary
   if (!(freed_ast = ast = (int *) malloc(poolsz)))
      die("could not allocate abstract syntax tree area");
   memset(ast, 0, poolsz); // not strictly necessary
   ast = (int *) ((int) ast + poolsz); // AST is built as a stack
   n = ast;

   // add primitive types
   tsize[tnew++] = sizeof(char);
   tsize[tnew++] = sizeof(int);
   tsize[tnew++] = sizeof(float);
   tsize[tnew++] = 0;  // reserved for another scalar type

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
         if (tk != Id) fatal("bad -D identifier");
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

   if (!(text = le = e = (char *) malloc(poolsz)))
      die("could not allocate text area");
   if (!(members = (struct member_s **)
                   malloc(SMALL_TBL_SZ * sizeof(struct member_s *))))
      die("could not malloc() members area");
   if (!(ef_cache = (struct ef_s **)
                    malloc(SMALL_TBL_SZ * sizeof(struct ef_s *))))
      die("could not malloc() external function cache");

   memset(e, 0, poolsz);

   memset(members, 0, SMALL_TBL_SZ * sizeof(struct member_s *));

   if (elf) elf32_init(poolsz); // call before source code parsing

   if (!(freep = lp = p = (char *) malloc(poolsz)))
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
