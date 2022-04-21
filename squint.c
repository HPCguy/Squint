/* Squint -- a peephole optimizer for mc.*/

/* mc.c uses a 2-register stack based VM. */
/* Squint converts this to a 2-register frame based VM. */
/* A 2-register frame based VM makes it easier to add */
/* a register allocation pass to the peephole optimizer. */

#include <stdio.h>
#include <stdlib.h>

#include <sys/types.h>
#include <sys/stat.h>
#include <fcntl.h>
#include <unistd.h>
#include <string.h>

/* Register Use/Def Analysis

   For each ARM instruction, create an integer descriptor describing
   register usage.  Since the stack simultaneously behaves like a
   single register *and* an 'infinite' register file, we also need to
   keep track of stack usage. We only track user-assignable registers
   here, and (mostly) exclude the behavior of the fp, sp, lr, pc whose
   behavior is constrained via both hardware and ABI conventions.  We
   do not track specific memory addresses, but do track use/def memOp.

   Lower descriptor bits are used to mirror the Rn/Rd/Rs/Rm register
   slots in an A32 opcode. This makes it easier to manipulate
   'active' register slots in the actual instructions.
*/

/*
   RI_memOp bits:

   000   No memory operation
   001   Inst   read
   010   Memory write
   011   Memory read
   100   Frame  write
   101   Frame  read
   110   Stack  write
   111   Stack  read

*/

#define RI_hasD    0xc0000000 /* Destination register */
#define RI_RdDest  0x80000000 /* Rd destination register */
#define RI_RnDest  0x40000000 /* Rn destination register */

#define RI_memOp   0x38000000 /* Mem descriptor */
#define RI_memMask 0x30000000 /* Mem op mask */
#define RI_memRW   0x08000000 /* Read(1)/Write(0) op */

#define RI_instR   0x08000000 /* Instruction mem read */
#define RI_dataW   0x10000000 /* Rd written to memory */
#define RI_dataR   0x18000000 /* Rd read  from memory */
#define RI_frameW  0x20000000 /* Rd written to frame var */
#define RI_frameR  0x28000000 /* Rd read  from frame var */
#define RI_push    0x30000000 /* Rd is pushed onto stack */
#define RI_pop     0x38000000 /* Rd is popped off of stack */

#define RI_bb      0x04000000 /* basic block boundary */
#define RI_float   0x02000000 /* a basic block ends */
#define RI_cond    0x01000000 /* indicates conditional execution */
#define RI_branch  0x00800000 /* branch instruction */
#define RI_func    0x00400000 /* function call */

#define RI_Active  0x000000f0 /* Bitmask for register usage in inst */
#define RI_RmAct   0x00000010
#define RI_RsAct   0x00000020
#define RI_RdAct   0x00000040
#define RI_RnAct   0x00000080

#define RI_Rm      0x0000000f /* register slots in ARM instruction */
#define RI_Rs      0x00000f00
#define RI_Rd      0x0000f000
#define RI_Rn      0x000f0000

/* NOP -- mov rn, rn */
#define NOP       0xe1a00000
#define NOP1      0xe1a01001
#define NOP13     0xe1a0d00d

enum search_dir { S_BACK = -1, S_FWD = 1 };

struct ia_s {
   int inst_addr; // inst address
   struct ia_s *next;
};

static struct pd_s {
   int data_addr; // data address
   struct ia_s *inst;
} *cnst_pool;
static int cnst_pool_size;

struct loop_s {
   struct loop_s *next;
   struct loop_s *nest; // nested loops
   int *begin;
   int *end;
   int info; // stats between begin/end
};

static int *cbegin;

/**********************************************************/
/*************** general utility functions ****************/
/**********************************************************/

static int popcount32b(int i)
{
   i = i - ((i >> 1) & 0x55555555); // add pairs of bits
   i = (i & 0x33333333) + ((i >> 2) & 0x33333333); // quads
   i = (i + (i >> 4)) & 0x0F0F0F0F; // groups of 8
   return (i * 0x01010101) >> 24; // horizontal sum of bytes
}

/**********************************************************/
/********** I-stream const pool utility functions  ********/
/**********************************************************/

/* The ARM processor allows use of pc-relative constants */

/* constants are sorted by the address they are stored in memory */
static int find_const(int addr)
{
   int low, high, mid;

   // pool is sorted by *relative address* of data values
   // with respect to beginning of text segment.
   // binsearch pool for address of data
   low = 0; high = cnst_pool_size;
   while (low != high) {
      mid = (low + high) / 2;
      if (cnst_pool[mid].data_addr <= addr) {
         low = mid;
         if (cnst_pool[mid].data_addr == addr) break;
         if (high-low == 1) ++low;
      }
      else
         high = mid;
   }
   return low;
}

static int is_const(int *inst)
{
   int addr = (inst-cbegin)*4;

   return (cnst_pool[find_const(addr)].data_addr == addr);
}

/* pc-relative constants always appear in the instruction */
/* stream after the instructions that reference them.  Because */
/* of this we can safely scan an instruction stream forward, */
/* skipping any constants we encounter. */
static void create_const_map(int *begin, int *end)
{
   int *scan = begin;
   int i;

   /* initialize global variables */
   cbegin = begin;
   cnst_pool = calloc(end-begin, sizeof(struct pd_s)); // 1/4 prog size
   cnst_pool_size = 0;

   while (scan < end) {
      if (!is_const(scan) &&
          ((i = (*scan & 0xffff0000) == 0xe59f0000) || // ldr  r0, [pc, #X]
           (*scan & 0xffff0f00) == 0xed9f0a00)) {      // vldr s0, [pc, #X]
         int offset = i ? ((*scan & 0xfff) / 4) : (*scan & 0xff);
         int addr = ((scan + 2 + offset) - begin)*4;
         struct ia_s **inst;
         int low = find_const(addr);

         if (cnst_pool[low].data_addr != addr) {
            int *mem = ((int *)&cnst_pool[cnst_pool_size]) + 1;

            // insert addr at this location
            for (i = cnst_pool_size; i > low; --i) {
               *mem = *(mem-2); --mem;
               *mem = *(mem-2); --mem;
            }
            cnst_pool[low].data_addr = addr;
            cnst_pool[low].inst = 0;
            ++cnst_pool_size;
         }
         /* keep track of pc-relative instructions that ref this constant */
         for (inst = &cnst_pool[low].inst; *inst != 0; inst = &(*inst)->next);
         *inst = malloc(sizeof(struct ia_s));
         (*inst)->inst_addr = (scan-begin)*4;
         (*inst)->next = 0;
      }
      ++scan;
   }
}

static void destroy_const_map()
{
   struct ia_s *inst, *next;
   int i;

   for (i = 0; i < cnst_pool_size; ++i) {
      inst = cnst_pool[i].inst;
      while (inst != 0) {
         next = inst->next;
         free(inst);
         inst = next;
      }
   }
   free(cnst_pool);
}

/* convert special ARM consts to mov immediate */
static void const_imm_opt(int *begin, int *end)
{
   struct ia_s *inst, *next;
   int *newinst;
   int i, j, val, rotate;

   for (i = 0, j = 0; i < cnst_pool_size; ++i) {
      val = cbegin[cnst_pool[i].data_addr/4];
      rotate = 0;
      if ((val & 0x0000ff00) && !(val & 0xffff00ff)) rotate = 1;
      if ((val & 0x00ff0000) && !(val & 0xff00ffff)) rotate = 2;
      if ((val & 0xff000000) && !(val & 0x00ffffff)) rotate = 3;

      if (rotate || (-256 <= val && val < 0)) {
         if (rotate) {
            val = (val >> (8*rotate)) & 0xff;
            rotate = (4-rotate)*1024;
         }
         inst = cnst_pool[i].inst;
         while (inst != 0) {
            next = inst->next;
            newinst = cbegin + inst->inst_addr/4;
            if ((*newinst & 0xffff0f00) == 0xed9f0a00) goto skip_fp; // vldr
            if (rotate) {
               *newinst = 0xe3a00000 | (*newinst & RI_Rd) | rotate | val; // mov
            }
            else {
               *newinst = 0xe3e00000 | (*newinst & RI_Rd) | -(val+1); // mvn
            }
            free(inst);
            inst = next;
         }
         cbegin[cnst_pool[i].data_addr/4] = NOP; // for safety
      }
      else {
skip_fp: if (i != j) {
            cnst_pool[j].data_addr = cnst_pool[i].data_addr;
            cnst_pool[j].inst      = cnst_pool[i].inst;
         }
         ++j;
      }
   }
   cnst_pool_size = j;
}

/* relocate a (v)ldr rX, [pc, #X] instruction */
static void rel_pc_ldr(int *dst, int *src)
{
   if (dst == src) return;

   int is_vldr = ((*src & 0xffff0f00) == 0xed9f0a00); // vldr
   if (is_vldr && ((src - dst + (*src & 0xff)) > 0xff)) {
      printf("Squint can't relocate vldr -- out of range\n");
      exit(-1);
   }

   struct ia_s **inst;
   int offset = is_vldr ? (*src & 0xff) : ((*src & 0xfff) / 4);
   int addr = ((src + 2 + offset) - cbegin)*4;
   int low = find_const(addr);
   if (cnst_pool[low].data_addr == addr) { // verify 'low' is index of const
      for (inst = &cnst_pool[low].inst; *inst != 0; inst = &(*inst)->next) {
         if ((*inst)->inst_addr == (src - cbegin)*4) {
            /* make sure pc-relative addr is still valid after move */
            (*inst)->inst_addr = (dst - cbegin)*4;
            *dst = *src + (src - dst)*(is_vldr ? 1 : 4);
            break;
         }
      }
   }
}

/* relocate a constant referenced by pc-relative instructions */
static void rel_pc_const(int *dst, int *src)
{
   /* assume no constants are added or removed */
   /* just migrated to a lower address, in order */
   if (dst < src) {
      struct ia_s **inst;
      int addr = (src - cbegin)*4;
      int low = find_const(addr);
      if (cnst_pool[low].data_addr == addr) { // verify 'low' is index of const
         int offset = (src - dst);
         cnst_pool[low].data_addr = (dst - cbegin)*4;
         for (inst = &cnst_pool[low].inst; *inst != 0; inst = &(*inst)->next) {
            int *scan = cbegin + (*inst)->inst_addr/4;
            int is_vldr = ((*scan & 0xffff0f00) == 0xed9f0a00); // vldr
            *scan -= (is_vldr ? offset : offset *4);
         }
         *dst = *src;
      }
   }
   else if (dst > src) {
      printf("addresses can be moved to a lower addr, not arbitrarily moved\n");
      exit(-1);
   }
}

/**********************************************************/
/************* NOP related utility functions **************/
/**********************************************************/

/* check for nop, PHD, and PHR0 instructions */
static int is_nop(int inst) {
   return ( (inst == NOP) || (inst == NOP1) || (inst == NOP13) );
}

/* skip any nop instructions in given direction. */
/* direction: -1 means move toward lower addresses */
/*             1 means move toward higher addresses */
/* Note that a NOP1 (PHD) will treat the instruction */
/* after it (+1) as though it were a nop */
static int *skip_nop(int *begin, enum search_dir dir)
{ /* -1 = backward, 1 = forward */
   int *scan = begin;
   int done;

   do {
      done = 1;

      /* skip past any consts in instruction stream */
      while (is_const(scan)) scan += dir;

      /* skip past any NOPS in instruction stream */
      if (*scan == NOP || *scan == NOP13) {
         scan += dir;
         done = 0;
      }
      else if (dir == 1 &&
               *scan == NOP1 &&
               (*(scan+1) & 0xffff0fff) != 0xe52d0004) { /* push */
         scan += 2;
         done = 0;
      }
      else if (*(scan-1) == NOP1 &&
               (*scan & 0xffff0fff) != 0xe52d0004 && /* push */
               dir == -1) {
         scan -= 2;
         done = 0;
      }
      else if (*scan == NOP1) {
         scan += dir;
         done = 0;
      }
   } while (!done);

   return scan;
}

/* This is a convenience function that should always */
/* start from a non-nop, non-const instruction. */
/* index is a count of how many non-nop instructions */
/* to move through the instruction stream. e.g. */
/* index == 0 means return immediately */
/* index == -5 means move backward 5 active instructions */
static int *active_inst(int *cursor, int index)
{
   enum search_dir dir;
   int count;

   if (index != 0) {
      if (index < 0) {
         dir = S_BACK;
         count = -index;
      }
      else {
         dir = S_FWD;
         count = index;
      }

      while (count > 0) {
         cursor += dir;
         cursor = skip_nop(cursor, dir);
         --count;
      }
   }

   return cursor;
}

/* This is executed immediately before compressing */
/* all nops out of the instruction stream */
static void rename_nop(int *begin, int *end)
{
   int *scan;

   for (scan = begin; scan <= end; ++scan) {
      if (is_nop(*scan) && !is_const(scan)) {
         *scan = NOP;
      }
   }
}

/**********************************************************/
/****************** use-def analysis ****************/
/**********************************************************/

/* The following bit masks are used to transfer 'active' register
   slots between the def/use descriptors and the A32 opcodes.
*/
static int activeRegMask[16] =
{
   0x00000000,
   0x0000000f,
   0x00000f00,
   0x00000f0f,
   0x0000f000,
   0x0000f00f,
   0x0000ff00,
   0x0000ff0f,
   0x000f0000,
   0x000f000f,
   0x000f0f00,
   0x000f0f0f,
   0x000ff000,
   0x000ff00f,
   0x000fff00,
   0x000fff0f
};

/**********************************************/

#define MAX_RENAME_REG 8

static int regtest(int inst, int opmask)
{
   return ((inst & opmask) == opmask);
}

/* funcBegin and funcEnd instructions guaranteed not to be NOP */
static void create_inst_info(int *instInfo, int *funcBegin, int *funcEnd)
{
   int *scan;
   int *rInfo = instInfo;
   int Rn, Rd, Rs, Rm;
   int inst, instMask, op, cond;

   for (scan = funcBegin; scan <= funcEnd; ++scan) {
      int info = 0;

      /* skip code that won't be transformed */
      if (is_nop(*scan)) {
         int *end = skip_nop(scan, S_FWD);
         while (scan < end) {
            if (*scan == NOP13) {
               *rInfo++ = RI_func; // R0 set in func
            }
            else
               *rInfo++ = info;
            ++scan;
         }
      }

      /* determine info about current instruction */
      inst = *scan;
      cond       = (inst >> 28) & 0x0f;
      instMask   = (inst >> 24) & 0x0e;
      Rn         = (inst >> 16) & 0x0f;
      Rd         = (inst >> 12) & 0x0f;
      Rs         = (inst >>  8) & 0x0f;
      Rm         =  inst        & 0x0f;

      /* Make note of registers eligible for renaming */
      if (cond < 0x0e) {  /* compiler can't produce cond == 0x0f inst */
         info |= RI_cond; /* conditionally executed inst */
      }

      if (instMask == 0x02) { /* ALU_immed */
         op = (inst >> 21) & 0x0f;
         if ((op != 0x0d) && (op != 0x0f)) {
             info |= RI_RnAct;
         }
         if ((op & 0x0c) != 0x08) { // not tst instructions
             info |= RI_RdAct | RI_RdDest;
         }
      }
      else if (instMask == 0x00) { /* ALU_register */
         if ((inst & 0xf0) == 0x90) { /* mul */
            info |= RI_RnAct | RI_RsAct | RI_RmAct | RI_RnDest;
         }
         else { /* not mul */
            op = (inst >> 21) & 0x0f;
            if ((op != 0x0d) && (op != 0x0f)) {
                info |= RI_RnAct;
            }
            if ((op & 0x0c) != 0x08) {
                info |= RI_RdAct | RI_RdDest;
            }
            if ((inst & 0x90) == 0x10) {
               info |= RI_RsAct;
            }
            info |= RI_RmAct;
         }
      }
      else if (instMask == 0x04 ||
               (instMask == 0x06 && (inst & (1<<4)) == 0)) { /* MEM op */
         int MEM_mask = (inst >> 20) & 0xd7;
         if (MEM_mask == 0x51 || MEM_mask == 0x55) { /* ldr || ldrb */
            info |= RI_RdAct | RI_RnAct | RI_RdDest;
            if (inst & (1<<25)) {
               info |= RI_RmAct;
            }

            if (Rn == 0x0f) {
               info |= RI_instR;
            }
            else if (Rn == 0x0b) {
               info |= RI_frameR;
            }
            else {
               info |= RI_dataR;
            }

            if (Rd == 0x0f) {
               info |= RI_branch;
            }
         }
         else if (MEM_mask == 0x50 || MEM_mask == 0x54) { /* str | strb */
            info |= RI_RdAct | RI_RnAct;
            if (Rn == 0x0b) {
               info |= RI_frameW;
            }
            else {
               info |= RI_dataW;
            }
         }
         else if (MEM_mask == 0x41) /* pop */
            info |= RI_RdAct | RI_RnAct | RI_RdDest | RI_pop;
         else if (MEM_mask == 0x52) /* push */
            info |= RI_RdAct | RI_RnAct | RI_push;
      }
      else if (instMask == 0x0a) { /* BRANCH (and link) */
         info |= RI_branch;
      }
      else if (instMask == 0x0c) { /* float */
         info |= RI_float;
         if ((inst & 0xffe00f00) == 0xed800a00)
            info |= RI_RnAct;
         if ((inst & 0xfff00f00) == 0xec500b00)
            info |= RI_RnAct | RI_RdAct | RI_RnDest | RI_RdDest;
      }
      else if (instMask == 0x0e) { /* float */
         info |= RI_float;
         if (((inst>>21) & 7) == 0)
            info |= RI_RdAct | ((inst & (1<<20)) ? (RI_RdDest) : 0);
      }

      /* Mask out any registers outside of rename range */

      if ( (Rn >= MAX_RENAME_REG) && regtest(info, RI_RnAct) ) {
         info &= ~(RI_RnAct | RI_RnDest);
      }
      if ( (Rd >= MAX_RENAME_REG) && regtest(info, RI_RdAct) ) {
         info &= ~(RI_RdAct | RI_RdDest);
         if ((info & RI_memMask) == RI_push) { /* check for push or pop */
            info &= ~RI_memOp; /* clear push/pop operation */
         }
         /* Note that RI_mem is left set here, since we still
            need to handle pc relative addresses */
      }
      if ( (Rs >= MAX_RENAME_REG) && regtest(info, RI_RsAct) ) {
         info &= ~RI_RsAct;
      }
      if ( (Rm >= MAX_RENAME_REG) && regtest(info, RI_RmAct) ) {
         info &= ~RI_RmAct;
      }

      /* keep register Ids for active registers */
      info |= inst & activeRegMask[(info & RI_Active) >> 4];

      *rInfo++ = info;
   }

   /* termination sentinel to simplify reg_info scans */
   *rInfo = 0xffffffff;
}

/* Mark block boundaries (after jump inst, or branch target inst) */
static void create_bb_info(int *instInfo, int *funcBegin, int *funcEnd)
{
   int *scan;
   int *rInfo = instInfo;

   for (scan = funcBegin; scan <= funcEnd; ++scan) {
      if ((*rInfo & RI_branch) == RI_branch) {
         if ((*rInfo & RI_cond) == RI_cond) {
            rInfo[1] = rInfo[1] | RI_bb;
         }
         if (((*scan >> 24) & 0x0f) == 0x0a) { /* local branch */
            int tmp = (*scan & 0x00ffffff) |
                      ((*scan & 0x00800000) ? 0xff000000 : 0);
            int *dst = scan + 2 + tmp;
            int off = dst - funcBegin;
            instInfo[off] = instInfo[off] | RI_bb;
         }
      }
      ++rInfo;
   }
}

/* find first def of reg in given direction */
static int *find_def(int *instInfo, int *rInfo, int reg, enum search_dir dir)
{
   int *retVal = 0;
   int info;

   while( (rInfo > instInfo) &&
          *rInfo != 0xffffffff ) { /* if in-bounds of func */
      info = *rInfo;
      if (info & RI_hasD) {
         int cmpReg = (info & RI_RnDest) ?
                      (info & RI_Rn) >> 16 : (info & RI_Rd) >> 12;
         if (reg == cmpReg) {
            retVal = rInfo;
            break;
         }
      }
      else if ((info & RI_func) && reg == 0) {
         retVal = rInfo; // only good for location, not content
         break;
      }
      rInfo += dir;
   }

   return retVal;
}

/* find first use of reg in given direction */
static int *find_use(int *instInfo, int *rInfo, int reg, enum search_dir dir)
{
   int *retVal = 0;
   int info;

   while( (rInfo > instInfo) &&
          *rInfo != 0xffffffff ) { /* if in-bounds of func */
      info = *rInfo;
      if (info & RI_hasD) {
         // if not a memory op or a memory read, disable dest reg
         if ((info & RI_memOp) == 0 || (info & RI_memRW)) {
            info ^= (info & RI_RnDest) ? RI_RnAct : RI_RdAct;
         }
      }
      if ((info & RI_RmAct) && (info & RI_Rm) == reg) {
         retVal = rInfo;
         break;
      }
      if (info & RI_RsAct && ((info & RI_Rs)>>8) == reg) {
         retVal = rInfo;
         break;
      }
      if (info & RI_RdAct && ((info & RI_Rd)>>12) == reg) {
         retVal = rInfo;
         break;
      }
      if (info & RI_RnAct && ((info & RI_Rn)>>16) == reg) {
         retVal = rInfo;
         break;
      }
      rInfo += dir;
   }

   return retVal;
}

/* pass in valid use and def pointer */
static int *find_use_precede_def(int *instInfo, int *use, int *def,
                                 int reg, enum search_dir dir)
{
   int *finalUse = use;

   if (def != 0) {
      do {
         finalUse = use;
         use = find_use(instInfo, use + dir, reg, dir);
      } while ( use != 0 && use < def);
   }
   if (use == def) finalUse = def;

   return finalUse;
}

static void reg_rename(int newreg, int oldreg, int *use, int *inst)
{
   int regSet = 0;
   int mask = (*use & RI_Active);

   if (*use & RI_hasD) // don't overwite dest reg
      mask &= ~((*use & RI_RdDest) ? RI_RdAct : RI_RnAct);

   if ((mask & RI_RmAct) && (*inst & RI_Rm) == oldreg)
      regSet |= newreg;
   else
      mask &= ~RI_RmAct;

   if ((mask & RI_RsAct) && ((*inst & RI_Rs) >> 8) == oldreg)
      regSet |= (newreg << 8);
   else
      mask &= ~RI_RsAct;

   if ((mask & RI_RdAct) && ((*inst & RI_Rd) >> 12) == oldreg)
      regSet |= (newreg << 12);
   else
      mask &= ~RI_RdAct;

   if ((mask & RI_RnAct) && ((*inst & RI_Rn) >> 16) == oldreg)
      regSet |= (newreg << 16);
   else
      mask &= ~RI_RnAct;

   if (mask & RI_Active) // rename registers
      *inst = (*inst & ~activeRegMask[mask >> 4]) | regSet;
}

/**********************************************************/
/********** peephole optimization funcs *******************/
/**********************************************************/

static void apply_peepholes1(int *funcBegin, int *funcEnd)
{
   int *scan;
   int *scanp1, *scanp2, *scanp3;
   for (scan = funcBegin; scan < funcEnd; ++scan) {
      scan = skip_nop(scan, S_FWD);
      /* one instruction peepholes */
      if (*scan == 0xe2400000 || /* sub r0, r0, #0 */
          *scan == 0xe2800000) { /* add r0, r0, #0 */
         *scan = NOP;
      }
      else {    /* two instruction peephole */

         scanp1 = active_inst(scan, 1);

         if ((*scan & 0xfff00f00) == 0xe2800000 || // add rI, rS, #X
             (*scan & 0xfff00f00) == 0xe2400000) { // sub rI, rS, #X
            int rS = (*scan >> 16) & 0xf; // Source, RI_Rn
            int rI = (*scan >> 12) & 0xf; // Index,  RI_Rd
            if ((*scanp1 & 0xff3000ff) == 0xe5100000 &&
                ((*scanp1 >> 16) & 0xf) == rI) { // ldr[b] rX, [rI]

               *scanp1 = (*scanp1 & 0xff70ff00) | (*scan & 0xff) |
                         (((*scan & 0xfff00000) == 0xe2800000) ? (1<<23) : 0) |
                         (rS << 16); // ldr[b] rX, [rS, #X]
               *scan = NOP;
            }
            else if (*scanp1 == 0xed900a00) { // vldr s0, [r0]
               *scanp1 = (*scanp1 & 0xff7fffff) | ((*scan & 0xff) / 4) |
                         (((*scan & 0xfff00000) == 0xe2800000) ? (1<<23) : 0) |
                         (rS << 16); // vldr s0, [rS, #X]
               *scan = NOP;
            }
         }
      }
   }

   funcEnd -= 2;
   for (scan = funcBegin; scan < funcEnd; ++scan) {
      scan = skip_nop(scan, S_FWD);
      scanp1 = active_inst(scan,   1);
      scanp2 = active_inst(scanp1, 1);

      /* change register to immediate if possible */
      if (*scan   == 0xe52d0004 && /* push {r0} */
          *scanp2 == 0xe49d1004 && /* pop  {r1} */
          (*scanp1 & 0xffffff00) == 0xe3a00000) { /* mov r0, #X */
         scanp3 = active_inst(scanp2, 1);
         if ((*scanp3 & 0xfe1fffff) == 0xe0010000 &&
             (*scanp3>>23 & 0x3) != 0x2) { /* exclude comparisons */
            *scanp3 = (*scanp3 ^ 1<<16) | 1<<25 | (*scanp1 & 0xff);
            *scan   = NOP;
            *scanp1 = NOP;
            *scanp2 = NOP;
            scan = scanp3;
         }
         else if (*scanp3 == 0xe1a00051) { /* asr r0, r1, r0 */
            int shift = ((*scanp1 & 0xff) < 0x20) ? (*scanp1 & 0x1f) : 0x1f;
            *scanp3 = 0xe1a00040 | (shift << 7);
            *scan   = NOP;
            *scanp1 = NOP;
            *scanp2 = NOP;
            scan = scanp3;
         }
      }
      else if ((*scan & 0xffffff00) == 0xe3a00000 && (*scan & 0xff) &&
               *scanp1 == 0xe3500000 && ((*scanp2 >> 24) & 0xff) == 0x1a) {
         *scan = NOP; *scanp1 = NOP;
         *scanp2 = 0xea000000 | (*scanp2 & 0x00ffffff);
         scan = scanp2;
      }
   }
   funcEnd += 2;
}

static void apply_peepholes2(int *instInfo, int *funcBegin, int *funcEnd)
{
   int *scan;
   int *scanp1, *scanp2, *scanp3, *scanp4, *scanp5;
   int *scanm1, *scanm2;

   create_inst_info(instInfo, funcBegin, funcEnd);

   funcEnd -= 6;
   for (scan = funcBegin; scan < funcEnd; ++scan) {
      scan = skip_nop(scan, S_FWD);
      /* Convert array addressing to more compact form */
      if ((*scan & 0xffffff00) == 0xe3a00000) { /* mov r0, #X */
         int imm = *scan & 0xff;
         scanm1 = active_inst(scan, -1);
         scanp1 = active_inst(scan,  1);
         if ((imm & (imm-1)) == 0 && // power of 2
             *scanm1 == 0xe52d0004 && /* push {r0} */
             *scanp1 == 0xe49d1004) { /* pop  {r1} */
            int *loc;
            scanm2 = active_inst(scanm1, -1);
            scanp2 = active_inst(scanp1,  1);
            scanp3 = active_inst(scanp2,  1);
            scanp4 = active_inst(scanp3,  1);
            loc = &instInfo[scanm2-funcBegin];
            if (find_def(instInfo, loc, 0, S_FWD) == loc &&
                               *scanp2 == 0xe0000091 && // mul r0, r1, r0
                               *scanp3 == 0xe49d1004 && // pop {r1}
                               *scanp4 == 0xe0810000) { // add r0, r1, r0
               int lev=1;
               int *pscan = scanm2;
               while (lev != 0 && pscan > funcBegin) {
                  pscan = skip_nop(--pscan, S_BACK);
                  if (*pscan == 0xe49d1004) { /* pop  {r1} */
                     ++lev;
                  }
                  else if (*pscan == 0xe52d0004) { /* push {r0} */
                     --lev;
                  }
               }
               if (pscan == funcBegin) {
                  printf("analysis error\n");
                  exit(-1);
               }
               else { // move onto "load" instruction
                  int *fscan = active_inst(pscan, -1);
                  if ((*fscan & 0xff700000) == 0xe5100000) { // ldr r0, [xxx]
                     imm = popcount32b(imm-1);
                     *pscan  = NOP;
                     *scanm1 = NOP;
                     *scan   = NOP;
                     *scanp1 = NOP;
                     *scanp2 = NOP;
                     *scanp3 = NOP;
                     *fscan |= 2 << 12; // dest = r2
                     scanp5 = active_inst(scanp4,  1);
                     // make sure no nested array access jk fix
                     loc = &instInfo[fscan-funcBegin];
                     if ((*scanp5 & 0xff3fffff) == 0xe5100000) {
                        // ldr[b] r0, [r0] -> ldr[b], [r2, r0, lsl #x]
                        *scanp4 = NOP;
                        *scanp5 = 0xe7920000 | (imm << 7);
                     }
                     else {
                        // add r0, r2, r0, lsl #X
                        *scanp4 = 0xe0820000 | (imm << 7);
                     }
                  }
                  scan = active_inst(scanp4, 1);
               }
            }
         }
      }
   }
   funcEnd += 6;
}

static void apply_peepholes3(int *instInfo, int *funcBegin, int *funcEnd)
{
   int *scan;
   int info;

   create_inst_info(instInfo, funcBegin, funcEnd);

   funcEnd -= 5;
   for (scan = funcBegin; scan < funcEnd; ++scan) {
      scan = skip_nop(scan, S_FWD);

      // get rid of unused postincrement-operator cruft
      info = instInfo[scan-funcBegin];
      if (info & RI_hasD) {
         int *next, *nextDef, *nextUse;
         int destR =
            ((info & RI_RdDest) ? (info&RI_Rd)>>12 : (info&RI_Rn)>>16) & 0xf;
         if (destR < 8) {
            next = active_inst(scan, 1);
            // if not a branch and not conditional execution...
            if (*next != 0xe28bd000 && // add   sp, fp, #0 -- LEV inst
                ((*next>>24) & 0x0e) != 0x0a && ((*next>>28) & 0x0f) == 0x0e &&
                *next != 0xe3a0780f && next[1] != 0xe3a0780f) { // CLCA
               int *loc = &instInfo[next-funcBegin];
               nextDef = find_def(instInfo, loc, destR, S_FWD);
               nextUse = find_use(instInfo, loc, destR, S_FWD);
               if (nextUse != 0 && nextDef != 0 && nextUse > nextDef) {
                  *scan = NOP;
               }
               scan = next-1;
            }
         }
      }
   }
   funcEnd += 5;
}

static void apply_peepholes4(int *funcBegin, int *funcEnd)
{
   int *scan, *scanp1;

   funcEnd -= 1;
   for (scan = funcBegin; scan <= funcEnd; ++scan) {
      scan = skip_nop(scan, S_FWD);
      scanp1 = active_inst(scan, 1);

      if (*scanp1 == 0xe0410000 &&              // sub r0, r1, r0
          (*scan & 0xffffff00) == 0xe3e00000 && // mvn r0, #X
          (*scan & 0xff) != 0xff) {  // -256
         *scanp1 = 0xe2810000 | ((*scan & 0xff) + 1); // add r0, r1, #(-X+1)
         *scan = NOP;
      }
      else if (*scan   == 0xe3e00000 && // mvn r0, #0
               *scanp1 == 0xe0000091) { // mul r0, r1, r0
         *scanp1 = 0xe2610000; // rsb r0, r1, #0
         *scan = NOP;
      }
      else if ((*scan & 0x0fffff00) == 0x03a00000 && // mov r0, #imm
               *scanp1 == 0xe1510000) {              // cmp r1, r0
         *scanp1 |= 2<<24 | (*scan & 0xff);
         *scan = NOP;
      }
   }
   funcEnd += 1;
}

static void apply_peepholes5(int *instInfo, int *funcBegin, int *funcEnd)
{
   int *scan, *scanm1, *scanp1, *scanp2;
   int *info, *rxd, *rxu, *rdd, *rdu, *rdt, *rfinal;
   int rx, rd, rxS;

   create_inst_info(instInfo, funcBegin, funcEnd);
   create_bb_info(instInfo, funcBegin, funcEnd);

   // op rx, ...
   // mov rd, rx   --->  op rd, ...
   for (scan = funcBegin; scan < funcEnd; ++scan) {
      scan = skip_nop(scan, S_FWD);

      if ((*scan & 0xffff0ff0) == 0xe1a00000) {
         rx = *scan & 0x0f;
         rd = (*scan >> 12) & 0x0f;
         if (rd < 3) continue; // extra precaution
         scanm1 = active_inst(scan, -1);
         rxS = (instInfo[scanm1-funcBegin] & RI_RdDest) ? 12 : 16;
         // need dep info to check dest reg for mul...
         if (((*scanm1 >> rxS) & 0x0f) == rx && ((*scanm1 >> 25) & 7) <= 2) {
            rxu = find_use(instInfo, &instInfo[scan-funcBegin]+1, rx, S_FWD);
            rxd = find_def(instInfo, &instInfo[scan-funcBegin]+1, rx, S_FWD);
            if (rxd == 0) rxd = &instInfo[funcEnd-funcBegin];
            if (rxu != 0 && rxu <= rxd) {
               rfinal = find_use_precede_def(instInfo, rxu, rxd, rd, S_FWD);
               for (rdt = &instInfo[(scan-funcBegin)];
                    rdt <= rfinal; ++rdt) if (*rdt & RI_bb) break;
               if (rdt > rfinal) {
                  do {
                     int *rscan = &funcBegin[rxu-instInfo];
                     reg_rename(rd, rx, rxu, rscan);
                     if (rxu == rfinal) break;
                     rxu = find_use(instInfo, &instInfo[rscan-funcBegin]+1,
                                    rx, S_FWD);
                  } while (1);
               }
            }
            if (rxu == 0 || rxu > rxd || rdt > rfinal) {
               *scan = (*scanm1 & ~(0x0f << rxS)) | (rd << rxS);
               *scanm1 = NOP;
            }
         }
      }
   }

   // mov rd, rx
   // op ..., rd   --->  op ..., rx
   for (scan = funcBegin; scan < funcEnd; ++scan) {
      scan = skip_nop(scan, S_FWD);

      if ((*scan & 0xffff0ff0) == 0xe1a00000) {
         rx = *scan & 0x0f;
         rd = (*scan >> 12) & 0x0f;
         scanp1 = active_inst(scan, 1);
         scanp2 = active_inst(scanp1, 1);
         if ((*scanp1 == 0xe3500000 && // switch stmt -- cmp r0, #0; bne x
             (((*scanp2 >> 24) & 255) == 0x1a)) ||
             rd > 2) {                 // frame var assignment
             continue;
         }
         info = &instInfo[scan-funcBegin] + 1;
         rdu = find_use(instInfo, info, rd, S_FWD);
         if (rd == 0 && rdu == 0) continue; // func return value
         rdd = find_def(instInfo, info, rd, S_FWD);
         if (rdd == 0) rdd = &instInfo[funcEnd-funcBegin];
         rfinal = find_use_precede_def(instInfo, rdu, rdd, rd, S_FWD);

         rxd = find_def(instInfo, info, rx, S_FWD);
         if (rxd == 0) rxd = &instInfo[funcEnd-funcBegin];
         rxu = find_use(instInfo, info, rx, S_FWD);
         if (rxu > rfinal) rxu = 0;
         else if (rxu != 0)
            rxu = find_use_precede_def(instInfo, rxu, rfinal, rx, S_FWD);

         if (rfinal <= rxd && rxd >= rfinal) {
            for (rdt = info; rdt <= rfinal; ++rdt) if (*rdt & RI_bb) break;
            if (rdt > rfinal) {
               do {
                  int *rscan = &funcBegin[rdu-instInfo];
                  reg_rename(rx, rd, rdu, rscan);
                  if (rdu == rfinal) break;
                  rdu = find_use(instInfo, &instInfo[rscan-funcBegin]+1,
                                 rd, S_FWD);
               } while (1);
               *scan = NOP;
            }
         }
      }
   }
}

// autoincrement pointer for load/str operations
static void apply_peepholes6(int *instInfo, int *funcBegin, int *funcEnd)
{
   int *scan, *iscan, *info;
   int *rdd, *rdu;
   int rn, inst;

   create_inst_info(instInfo, funcBegin, funcEnd);
   create_bb_info(instInfo, funcBegin, funcEnd);

   for (scan = funcBegin; scan < funcEnd; ++scan) {
      scan = skip_nop(scan, S_FWD);

      if ((*scan & 0xff200fff) == 0xe5000000) { // (ldr|str)[b] rd, [rn]
         rn = (*scan >> 16) & 0x0f;
         info = &instInfo[scan-funcBegin];
         rdu = find_use(instInfo, info+1, rn, S_FWD);
         rdd = find_def(instInfo, info+1, rn, S_FWD);
         if (rdd == 0) rdd = &instInfo[funcEnd-funcBegin];
         if (rn < 10 && (rdu == 0 || rdu > rdd)) {
            rdd = find_def(instInfo, info-1, rn, S_BACK);
            rdu = find_use(instInfo, info-1, rn, S_BACK);
            if (rdu == 0 || rdu <= rdd) {
               iscan = rdd+1;
               while ( iscan <= info && (*iscan & RI_bb) == 0) ++iscan;
               if (iscan > info) {
                  inst = funcBegin[rdd-instInfo]; // sub rn, rx, #X
                  iscan = find_def(instInfo, rdd+1, (inst >> 16) & 0x0f, S_FWD);
                  if ((inst & 0xfff0ff00) == (0xe2400000 | (rn << 12)) &&
                      (iscan == 0 || iscan > info)) {
                     *scan = (*scan & 0xff70ffff) | (inst & 0x000f00ff);
                     funcBegin[rdd-instInfo] = NOP;
                  }
               }
            }
         }
      }
   }

   create_inst_info(instInfo, funcBegin, funcEnd);
   create_bb_info(instInfo, funcBegin, funcEnd);

   for (scan = funcBegin; scan < funcEnd; ++scan) {
      scan = skip_nop(scan, S_FWD);

      if ((*scan & 0xff200f00) == 0xe5000000) { // (ldr|str)[b] rd, [rn]
         rn = (*scan >> 16) & 0x0f;
         if (rn >= 10) continue;
         info = &instInfo[scan-funcBegin];
         rdd = find_def(instInfo, info-1, rn, S_BACK);
         rdu = find_use(instInfo, info-1, rn, S_BACK);
         if (rdu == rdd) {
            iscan = rdd+1;
            while ( iscan <= info && (*iscan & RI_bb) == 0) ++iscan;
            inst = funcBegin[rdd-instInfo];
            // TODO: allow add or subtract #x inst
            if (iscan > info && (inst & 0xfff00000) == 0xe2800000) { // add
               int off = inst & 0xff;
               if ((*scan & 0xff) == off &&
                   ((*scan & (1<<22)) ? (off == 1) : (off == 4)) ) {
                  // TODO: immediate value should be #X rather than 1 or 4
                  // add r5, r5, #1
                  // str rd, [r5, #-1] -> str rd, [r5], #1
                  if ((*scan & (1<<23)) == 0) {
                     funcBegin[rdd-instInfo] = NOP;
                     *scan = (*scan & 0xfedfffff) | (1<<23);
                  }
               }
               else if ((*scan & 0xff) == 0) {
                  // add r5, r5, #1
                  // str rd, [r5]   -> str rd, [r5, #1]!
                  funcBegin[rdd-instInfo] = NOP;
                  *scan = *scan | (5<<21) | (inst & 0xff);
               }
            }
         }
      }
   }
}

static void apply_peepholes7(int *instInfo, int *funcBegin, int *funcEnd)
{
   int *scan, *scanp1;
   int *rdt, *info, *rfinal;

   create_inst_info(instInfo, funcBegin, funcEnd);
   create_bb_info(instInfo, funcBegin, funcEnd);

   for (scan = funcBegin; scan < funcEnd; ++scan) {
      scan = skip_nop(scan, S_FWD);

      if (*scan == 0xe52d0004) { // push {r0}
         scanp1 = active_inst(scan,1);
         if ((*scanp1 & 0xffff0fff) == 0xe49d0004 && // pop {rn}
             (instInfo[scanp1-funcBegin] & RI_bb) == 0) {
            int rd = (*scanp1 >> 12) & 0x0f;
            if (rd > 9) continue;
            if (rd == 0) {
               *scan   = NOP;
               *scanp1 = NOP;
               scan = scanp1;
               continue;
            }
            else if ((instInfo[scan-funcBegin] & RI_bb) == 0) {
               int *scanm1 = active_inst(scan,-1);
               int iinfo = instInfo[scanm1-funcBegin];
               if (iinfo & RI_hasD) {
                  int off = (iinfo & RI_RdDest) ? 12 : 16;
                  if (((*scanm1 >> off) & 0x0f) == 0) {
                     *scanm1 = *scanm1 | (rd << off);
                     *scan   = NOP;
                     *scanp1 = NOP;
                     scan = scanp1;
                     continue;
                  }
               }
            }

            // default action
            *scan   = NOP;
            *scanp1 = 0xe1a00000 | (rd << 12);
            scan = scanp1;
         }
      }
   }

   --funcEnd;
   for (scan = funcBegin; scan < funcEnd; ++scan) {
      scan = skip_nop(scan, S_FWD);

      if ((*scan & 0xffffff00) == 0xe50b0000) { // str r0, [fp, #X]
         scanp1 = active_inst(scan,1);
         if ((*scanp1 & 0xffffff00) == 0xe51b0000 && // ldr r0, [fp, #X]
             (*scan & 0xff) == (*scanp1 & 0xff)) {
            info = &instInfo[scan-funcBegin]+1;
            rfinal = &instInfo[scanp1-funcBegin];
            for (rdt = info; rdt <= rfinal; ++rdt) if (*rdt & RI_bb) break;
            if (rdt > rfinal) {
               *scanp1 = NOP;
               scan = scanp1;
            }
         }
         else if ((*scanp1 & 0xffffff00) == 0xe51b1000 && // ldr r1, [fp, #X]
             (*scan & 0xff) == (*scanp1 & 0xff)) {
            info = &instInfo[scan-funcBegin]+1;
            rfinal = &instInfo[scanp1-funcBegin];
            for (rdt = info; rdt <= rfinal; ++rdt) if (*rdt & RI_bb) break;
            if (rdt > rfinal) {
               int *scanp2 = active_inst(scanp1, 1);
               rfinal = &instInfo[scanp2-funcBegin];
               for (; rdt <= rfinal; ++rdt) if (*rdt & RI_bb) break;
               if (rdt > rfinal) {
                  if ((*scanp2 & 0xffffff00) == 0xe3510000) { // cmp r1, #x
                     *scanp1 = NOP;
                     *scanp2 -= 0x10000;
                     scan = scanp2;
                  }
                  else
                     goto fallback;
               }
               else {
fallback:
                  *scanp1 = 0xe1a01000; // mov r1, r0
                  scan = scanp1;
               }
            }
         }
      }
   }
   ++funcEnd;
}

/**********************************************************/
/************* branch code optimizations ******************/
/**********************************************************/

/* remove unreachable code by setting to NOP */
static void simplify_branch1(int *funcBegin, int *funcEnd)
{
   int **queue;
   char *reachable;
   int *scan;
   int numQueue = 1;
   int cursor;

   /* flag array, 0 = unreachable */
   reachable = (char *) malloc((funcEnd-funcBegin)+1);
   memset(reachable, 0, (funcEnd-funcBegin)+1);

   /* estimate number of branches */
   for (scan = funcBegin; scan <= funcEnd; ++scan) {
      if ((*scan & 0x0e000000) == 0x0a000000) { // branch
         ++numQueue;
      }
   }

   queue = (int **) malloc(numQueue*sizeof(int *));
   numQueue = 0;
   queue[numQueue++] = funcBegin;

   while (numQueue > 0) {
      while (numQueue > 0 &&
             reachable[(scan = queue[--numQueue]) - funcBegin] );

      if (reachable[scan-funcBegin])
         break;

      do {
         reachable[scan-funcBegin] = 1; // 1 = inst
         if ((*scan & 0x0f000000) == 0x0a000000) { // branch
            int cond = (*scan>>28) & 0xf;
            if (cond < 0xf) { // extended instruction == 0x0f
               int i;
               int tmp = (*scan & 0x00ffffff) |
                         ((*scan & 0x00800000) ? 0xff000000 : 0);
               int *dst = scan + 2 + tmp;
               if (!reachable[dst-funcBegin]) {
                  for (i = 0; i < numQueue; ++i) {
                     if (queue[i] == dst)
                        break;
                  }
                  if (i == numQueue) {
                     queue[numQueue++] = dst;
                  }
               }
            }
            if (cond == 0x0e) { // unconditional
               break;
            }
         }
         else if (*scan == 0xe8bd8800) // pop {fp, pc}
            break;

         ++scan;
      } while (scan <= funcEnd);
   }

   cursor = 0;
   for (scan = funcBegin; scan <= funcEnd; ++scan) {
      if (!reachable[cursor++]) {
         if (!is_const(scan)) {
            *scan = NOP;
            --scan;
            if ((*scan & 0x0fffffff) == 0x0a000000 &&
                ((*scan >> 28) & 0xf) != 0xf && !is_const(scan)) {
               *scan = NOP; // remove branch past single nop
            }
            --scan;
            if ((*scan & 0x0fffffff) == 0x0a000001 &&
                ((*scan >> 28) & 0xf) != 0xf && !is_const(scan)) {
               *scan ^= 1; // allows other optimizations in mc
            }
            ++scan;
            ++scan;
         }
      }
      else if (*scan == 0xeaffffff) { // branch to next statement
         *scan = NOP;
      }
   }

   free(queue);
   free(reachable);
}

/* recursively follow a chain of unconditional branches */
/* add set all branch targets to the final address */
static int *rethread_branch(int *branchInst)
{
   int* retVal;
   if  ((*branchInst & 0xff000000) == 0xea000000) { // uncond branch
      int tmp = (*branchInst & 0x00ffffff) |
                ((*branchInst & 0x00800000) ? 0xff000000 : 0);

      int *dstInst = branchInst + 2 + tmp;
      retVal = rethread_branch(dstInst);
      *branchInst += (retVal - dstInst);
   }
   else {
      retVal = branchInst;
   }
   return retVal;
}

/* rethread unconditional branch chains */
static void simplify_branch2(int *funcBegin, int *funcEnd) {
   int *scan;
   for (scan = funcBegin; scan <= funcEnd; ++scan) {
      scan = skip_nop(scan, S_FWD);
      if ((*scan & 0xff000000) == 0xea000000) { // uncond branch
         rethread_branch(scan);
      }
   }
}

/* mc has condtional branches that jump to a compare instruction */
/* follow chains of these instructions to go directly to final address */
static void simplify_branch3(int *funcBegin, int *funcEnd) {
   int *scan;
   int match, tmp;
   for (scan = funcBegin; scan < funcEnd; ++scan) {
      scan = skip_nop(scan, S_FWD);
      if (*scan == 0xe3500000) { /* cmp r0, #0 */
         int *branchInst = active_inst(scan, 1);
         int *target = branchInst;
         if ((*target & 0xff000000) == 0x0a000000 || /* beq */
             (*target & 0xff000000) == 0x1a000000) { /* bne */
            match = (*target & 0xf0000000);
            while(1) {
               /* calc sign extended branch addr */
               tmp = (*target & 0x00ffffff) |
                     ((*target & 0x00800000) ? 0xff000000 : 0);
               target += 2 + tmp;
               if (*target == 0xe3500000) { /* cmp r0, #0 */
                  target = active_inst(target, 1);
               }
               if ((*target & 0xff000000) == 0x0a000000 || /* beq */
                   (*target & 0xff000000) == 0x1a000000) { /* bne */
                  if ((*target & 0xf0000000) != match) {
                     ++target;
                     if ((*target & 0xff000000) != 0xea000000) { /* b */
                        break;
                     }
                  }
               }
               else if ((*target & 0xff000000) != 0xea000000) { /* b */
                  break;
               }
            }
            *branchInst = (*branchInst & 0xff000000) |
                          ((target - branchInst - 2) & 0xffffff);
         }
      }
   }
}

/* Now that we have simplified all final branch targets, we */
/* will simplify and compress out extraneous comparison opcodes. */
static void simplify_branch4(int *funcBegin, int *funcEnd)
{
   int *scan;
   for (scan = funcBegin; scan < funcEnd; ++scan) {
      scan = skip_nop(scan, S_FWD);

      if (*scan == 0xe3500000) { /* cmp r0, #0 */
         int *scanm1, *scanm2, *scanm3, *scanp1;

         /* simplify cmp blocks */
         scanm1 = active_inst(scan,   -1);
         scanm2 = active_inst(scanm1, -1);
         scanm3 = active_inst(scanm2, -1);
         if ((*scanm3 & 0xf3f0f000) == 0xe1500000)  { /* cmp rX, r0 */
            scanp1 = active_inst(scan, 1);
            int match = ((*scanp1 & 0xf0000000) == 0) ? 0 /* eq */ : 1 /* ne */;
            if ((*scanm1 & 1) == match) {
               *scanp1 = (*scanp1 & 0x0fffffff) | (*scanm1 & 0xf0000000);
            }
            else {
               *scanp1 = (*scanp1 & 0x0fffffff) | (*scanm2 & 0xf0000000);
            }
            *scanm2 = NOP;
            *scanm1 = NOP;
            *scan   = NOP;
         }
      }
   }
}

/* "bcond target1 ; b target2 ; taget1: <inst>;" -> "bnotcond target2;" */
/* todo: may want to check for chains of intervening nops here */
static void simplify_branch5(int *funcBegin, int *funcEnd)
{
   int *scan;
   for (scan = funcBegin; scan < funcEnd; ++scan) {
      scan = skip_nop(scan, S_FWD);
      if ((*scan & 0x0fffffff) == 0x0a000000 &&  // cond jump past next inst
          ((*scan >> 28) &0xf) < 0xe &&
          (scan[1] & 0xff000000) == 0xea000000) {
         scan[1] = (scan[0] & 0xff000000) | (scan[1] & 0x00ffffff);
         scan[1] = scan[1] ^ (int) 0x10000000;
         scan[0] = NOP;
      }
   }
}


/* optimize branch-specific code sequences */
static void simplify_branch(int *funcBegin, int *funcEnd) {
   simplify_branch1(funcBegin, funcEnd);
   simplify_branch2(funcBegin, funcEnd);
   simplify_branch3(funcBegin, funcEnd);
   simplify_branch4(funcBegin, funcEnd);
   simplify_branch5(funcBegin, funcEnd);
   simplify_branch1(funcBegin, funcEnd); // optional
}

/* remove all NOP instructions ( mov r0, r0 ) and adjust branches */
/* mode == 0: intra-function repack, mode != 0, inter-function */
static int *relocate_nop(int *funcBegin, int *funcEnd, int mode)
{
   int *retVal = funcEnd;
   int *memblk;
   int *branchAddr;
   int *branchTarget;
   int *permutation;
   int ii, jj, tmp, done;
   int currAddr;
   int currTarget;
   int hasLink;
   int offset;
   int align;

   int *scan;
   int *packed;

   /* count number of branches to relocate */
   int nopCount = 0;
   int branchCount = 0;

   for (scan = funcBegin; scan < funcEnd; ++scan) {
      while (is_const(scan)) ++scan;

      // legal since rename_nop occurs before this call...
      if (*scan == NOP) {
         ++nopCount;
      }
      else if ((*scan & 0x0e000000) == 0x0a000000) {
         hasLink = *scan & (1<<24);
         if ( ((mode == 0) && !hasLink) ||   // intra-function mode
              ((mode != 0) &&  hasLink) ) {  // inter-function mode
            ++branchCount;
         }
      }
   }

   if (branchCount != 0) {
      memblk = malloc(3*branchCount*4);
      branchAddr = memblk;
      branchTarget = branchAddr + branchCount;
      permutation = branchTarget + branchCount;

      /* record all branch inst and target addresses */
      branchCount = 0;
      for (scan = funcBegin; scan < funcEnd; ++scan) {
         while (is_const(scan)) ++scan;

         if ((*scan & 0x0e000000) == 0x0a000000) {
            hasLink = *scan & (1<<24);
            if ( ((mode == 0) && !hasLink) ||   // intra-function mode
                 ((mode != 0) &&  hasLink) ) {  // inter-function mode
               /* add branch to table */
               permutation[branchCount] = branchCount;
               branchAddr[branchCount] = (scan - funcBegin);
               tmp = (*scan & 0x00ffffff) |
                     ((*scan & 0x00800000) ? 0xff000000 : 0);
               branchTarget[branchCount] = (scan - funcBegin) + 2 + tmp;
               ++branchCount;
            }
         }
      }

      /* sort in terms of target address */
      /* this is an order preserving sort */
      /* for secondary keys */
      ii = 0;
      do {
         done = 1;
         for (jj = branchCount-2; jj >= ii; --jj) {
            if (branchTarget[jj+1] < branchTarget[jj]) {
               tmp = branchTarget[jj+1];
               branchTarget[jj+1] = branchTarget[jj];
               branchTarget[jj] = tmp;
               tmp = permutation[jj+1];
               permutation[jj+1] = permutation[jj];
               permutation[jj] = tmp;
               done = 0;
            }
         }
         ++ii;
      } while (!done);
   }

   if (nopCount > 0) { // there are nops to remove

      currAddr = 0;
      currTarget = 0;
      offset = 0;

      packed = funcBegin;
      for (scan = funcBegin; scan <= funcEnd; ++scan, ++offset) {
         align = 1;
         while (currTarget < branchCount) {
            if (branchTarget[currTarget] == offset) {
               if (align && ((mode != 0) ||
                   branchAddr[permutation[currTarget]] > offset)) {
                  /* quadword align loop branch target */
                  tmp = 4 - ((packed-funcBegin) & 3);
                  if (tmp != 4 && (scan-packed) >= tmp) {
                     while (tmp-- > 0)
                        *packed++ = NOP;
                  }
                  align = 0;
               }
               branchTarget[currTarget++] = packed - funcBegin;
            }
            else {
               break;
            }
         }
         if (currAddr < branchCount) {
            if (branchAddr[currAddr] == offset) {
               branchAddr[currAddr++] = packed - funcBegin;
            }
         }
         if (is_const(scan)) {
            rel_pc_const(packed, scan);
            ++packed;
         }
         else if (*scan != NOP) {
            if ((*scan & 0xffff0000) == 0xe59f0000 || // ldr  rN, [pc, #X]
                (*scan & 0xffff0f00) == 0xed9f0a00) { // vldr sN, [pc, #X]
               rel_pc_ldr(packed, scan);
               ++packed;
            }
            else if ((*scan & 0x0e000000) == 0x0a000000) {
               if (*scan & (1<<24)) {
                  // adjust bl instruction address
                  // jk this is bogus since for mode 0 only
                  *scan += scan - packed;
               }
               *packed++ = *scan;
            }
            else
               *packed++ = *scan;
         }
      }

      /* fixup branch instructions with new target address */
      for (ii = 0; ii < branchCount; ++ii) {
         tmp = branchAddr[permutation[ii]];
         funcBegin[tmp] = (funcBegin[tmp] & 0xff000000) |
                      ((branchTarget[ii] - tmp - 2) & 0x00ffffff);
      }

      retVal = packed - 1;

      while (packed<=funcEnd) {
         *packed++ = NOP;
      }
   }

   if (branchCount != 0) {
      free(memblk);
   }

   return retVal;
}

/**********************************************************/
/**** Eliminate nested push/pop pairs where possible ******/
/**********************************************************/

static struct { int *push, *pop; int lev; } pair[2000];

static void create_pushpop_map(int *instInfo, int *funcBegin, int *funcEnd)
{
   int i, lev = 0;
   int maxlev = 0;
   int *scanm1, *scanp1;
   int *stack[10];
   int *scan;
   int np = 0;

   create_inst_info(instInfo, funcBegin, funcEnd);
   for (scan = funcBegin; scan < funcEnd; ++scan) {
      scan = skip_nop(scan, S_FWD);

      if (*scan == 0xe52d0004 && // push {r0}
          *(scan-1) != NOP1 &&
          *(scan+3) != 0xe3a0780f && // mov r7, #983040
          *(scan+6) != 0xe3a0780f) { // mc specific hack
         stack[lev] = scan;
         ++lev;
      }
      else if (*scan == 0xe49d1004 && // pop {r1}
               *(scan-1) != NOP1) {
         --lev;
         pair[np].push = stack[lev];
         pair[np].pop  = scan;
         pair[np].lev  = lev;
         if (lev > maxlev) maxlev = lev;
         if (++np == 2000) {
            printf("pushpop overflow\n");
            exit(-1);
         }
      }
   }

   // innermost to outermost push/pop pairs
   for (i = 0; i < np; ++i) {
      scanm1 = active_inst(pair[i].push, -1);
      scanp1 = active_inst(pair[i].pop,   1);
      if (*scanp1 == 0xe5810000 || *scanp1 == 0xe5c10000 || // str[b] r0, [r1]
          *scanp1 == 0xed810a00) { // vstr s0, [r1]
         int *pushp1 = &instInfo[(pair[i].push-funcBegin)+1];
         int *r0d = find_def(instInfo, pushp1, 0, S_FWD);
         int *r0u = find_use(instInfo, pushp1, 0, S_FWD);
         if (r0d <= r0u &&
             ((*scanm1 & 0xffffff00) == 0xe28b0000 ||  // add r0, fp, #X
              (*scanm1 & 0xffffff00) == 0xe24b0000 )) { // sub r0, fp, #X
            int off = *scanm1 & 0xff;
            int addOffsetBit = 1<<23;
            if ((*scanm1 & 0xffffff00) == 0xe24b0000) { // sub r0, fp, #X
               addOffsetBit = 0;
            }
            if (r0u == r0d) {
               if (funcBegin[r0d-instInfo] == 0xe5900000 || //  ldr r0, [r0]
                   funcBegin[r0d-instInfo] == 0xed900a00) { // vldr s0, [r0]
                  funcBegin[r0d-instInfo] =
                     (funcBegin[r0d-instInfo] & 0xff70ff00) | addOffsetBit |
                      0x000b0000 | ((*scanp1 == 0xed810a00) ? (off / 4) : off);
               }
               else {
                  continue;
               }
            }
            *scanm1 = NOP;
            *pair[i].push = NOP;
            *pair[i].pop  = NOP;
            *scanp1 = (*scanp1 & 0xff70ff00) | addOffsetBit |
                      0x000b0000 | ((*scanp1 == 0xed810a00) ? (off / 4) : off);
         }
         else if (r0d < r0u &&
                  (*scanm1 & 0xffff0000) == 0xe59f0000 && // ldr r0, [pc, #X]
                  (scanm1 + 2 + (*scanm1 & 0xfff)/4) > pair[i].pop) {
            rel_pc_ldr(pair[i].pop, scanm1);
            *pair[i].pop |= 1<<12;
            *scanm1 = NOP;
            *pair[i].push = NOP;
         }
      }
   }

   lev=0;
   maxlev=0;
   np=0;
   scan=funcBegin;

   create_inst_info(instInfo, funcBegin, funcEnd); // regen dependency info
   while (scan < funcEnd) {
      scan = skip_nop(scan, S_FWD);

      if (*scan == 0xe52d0004 && // push {r0}
          *(scan-1) != NOP1 &&
          *(scan+3) != 0xe3a0780f &&
          *(scan+6) != 0xe3a0780f) {
         stack[lev] = scan;
         ++lev;
      }
      else if (*scan == 0xe49d1004 && // pop {r1}
               *(scan-1) != NOP1) {
         --lev;
         pair[np].push = stack[lev];
         pair[np].pop  = scan;
         pair[np].lev  = lev;
         if (lev > maxlev) maxlev = lev;
         if (++np == 2000) {
            printf("pushpop overflow\n");
            exit(-1);
         }
      }
      ++scan;
   }

   // outermost to innermost push/pop pairs
   // need a ldr r0, [pc, #x] guard here for scanm1 instruction ...
   for (lev = 0; lev <= maxlev; ++lev) {
      for (i = 0; i < np; ++i) {
         if (pair[i].lev == lev) {
            int *pop  = &instInfo[pair[i].pop-funcBegin];
            int *pushp1 = &instInfo[(pair[i].push-funcBegin)+1];
            int *r1push1u = find_use(instInfo, pushp1, 1, S_FWD);
            int *r1push1d = find_def(instInfo, pushp1, 1, S_FWD);
            for (scan = pair[i].push + 1; scan < pair[i].pop; ++scan) {
               if (*scan == NOP13 && !is_const(scan)) break; // func call
            }
            if (scan != pair[i].pop) continue; // skip regions with func call

            /* if r1 not used or defined between push and pop... */
            if (r1push1d == pop && r1push1u > pop) {
               scanm1 = active_inst(pair[i].push, -1);
               int *m1 = &instInfo[scanm1-funcBegin];
               int *r0md = find_def(instInfo, m1, 0, S_FWD);
               int *r0push1u = find_use(instInfo, pushp1, 0, S_FWD);
               int *r0push1d = find_def(instInfo, pushp1, 0, S_FWD);
               int m1modifiable =
                  (*(pair[i].push - 1) != NOP13) && (r0md == m1);

               /* if r0 defined in instruction before push and */
               /* within push/pop, def of r0 happens before use... */
               if (m1modifiable && r0push1u > r0push1d) {
                  if ((*scanm1 & 0xf0000000) == 0xe0000000) { // uncond op
                     *scanm1 |=
                        (((*scanm1 & 0x0e0000f0) == 0x90) ? (1<<16) : (1<<12));
                     *pair[i].push = NOP;
                     *pair[i].pop  = NOP;
                  }
               }
               else {
                  *pair[i].push = 0xe1a01000; // mov r1, r0
                  *pair[i].pop  = NOP;
               }
            }
         }
      }
   }
}

/* simple functions with no locals do not need a frame */
void simplify_frame(int *funcBegin, int *funcEnd)
{
   int *scan, *scanp1;
   if (funcBegin[1] == 0xe28db000 && // add  fp, sp, #0
       (funcBegin[2] & 0xfffff000) != 0xe24dd000) { // sub sp, sp, #X
      int fo = -4 ; // frame offset
      for (scan = funcBegin; scan <= funcEnd; ++scan) {
         scan = skip_nop(scan, S_FWD);

         if (*scan == 0xe92d4800) { // push  {fp, lr}
            *scan = 0xe92d4000; // push {lr}
         }
         else if (*scan == 0xe8bd8800) { // pop  {fp, pc}
            *scan = 0xe8bd8000; // pop {pc}
         }
         else if (*scan == 0xe28db000) { // add  fp, sp, #0
            *scan = NOP;
         }
         else if (*scan == 0xe28bd000) { // add   sp, fp, #0
            *scan = NOP;
         }
         else if (*scan == 0xe28dd004) { // add sp, sp, #4
            scanp1 = active_inst(scan, 1);
            if (*scanp1 == 0xe52d0004) { // push {r0}
              *scan = NOP;
              *scanp1 = 0xe58d0000; // str r0, [sp]
            }
         }
         else if ((*scan & 0xfffff000) == 0xe28dd000) { // add sp, sp, #X
            fo -= *scan & 0xfff;
         }
         else if ((*scan & 0xffff0004) == 0xe52d0004) { // push {rX}
            fo += 4;
         }
         else if ((*scan & 0xffff0004) == 0xe49d0004) { // pop {rX}
            fo -= 4;
         }
         else if ((*scan & 0xffbf0000) == 0xe59b0000) { // ldr[b] rX, [fp, #Y]
            *scan += 0x20000 + fo; // ldr[b] rX, [sp, #Y]
         }
         else if ((*scan & 0xffbf0000) == 0xe58b0000) { // str[b] rX, [fp, #Y]
            *scan += 0x20000 + fo; // str[b] rX, [sp, #Y]
         }
      }
   }
}


/**********************************************************/
/****       convert frame vars to registers          ******/
/**********************************************************/

void rename_register1(int *instInfo, int *funcBegin, int *funcEnd)
{
#define BR 3  /* base register */
#define MAX_REN_REG 7
   int *scan;
   int offset[MAX_REN_REG];
   int i, j, numReg = 0;

   /* Abort trivial reg renaming if function calls exist */
   for (scan = funcBegin; scan <= funcEnd; ++scan)
      if (*scan == NOP13 && !is_const(scan)) return;

   /* record frame variable in this context */
   for (scan = funcBegin; scan <= funcEnd; ++scan) {
      scan = skip_nop(scan, S_FWD);

      if ((*scan & 0x0f2f0000) == 0x050b0000) { // (ldr|str)[b], [fp, #X]
         int off = (*scan & 0xfff);
         if  ((*scan & (1<<23)) == 0) off = -off;
         for (i = 0; i < numReg; ++i) {
            if (offset[i] == off) break;
         }
         if (i == numReg) {
            offset[numReg++] = off;
            if (numReg == MAX_REN_REG) break;
         }
      }
   }

   /* discard any frame variables that are not trivial */
   for (scan = funcBegin; scan <= funcEnd; ++scan) {
      scan = skip_nop(scan, S_FWD);

      if ((*scan & 0xffffff00) == 0xe28b0000 || // add r0, fp, #X
          (*scan & 0xffffff00) == 0xe24b0000) { // sub r0, fp, #X
         int off = *scan & 0xff;
         if ((*scan & 0xffffff00) == 0xe24b0000) {
            off = -off;
         }
         for (i = 0; i < numReg; ++i) {
            if (offset[i] == off) break;
         }
         if (i != numReg) {
            --numReg;
            for(; i < numReg; ++i)
               offset[i] = offset[i+1];
         }
      }
   }

   if (numReg == 0) return;

   create_inst_info(instInfo, funcBegin, funcEnd); // dependency info
   create_bb_info(instInfo, funcBegin, funcEnd);

   /* create registers for frame vars */
   for (scan = funcBegin; scan <= funcEnd; ++scan) {
      scan = skip_nop(scan, S_FWD);

      if ((*scan & 0x0f2f0000) == 0x050b0000) { // (ldr|str)[b], [fp, #X]
         int rd;
         int *rdu, *rdd, *rdt, *rfinal;
         int off = (*scan & 0xfff);
         if  ((*scan & (1<<23)) == 0) off = -off;

         for (i = 0; i < numReg; ++i) {
            if (offset[i] == off) break;
         }
         if (i == numReg) continue; // this frame var not mapped

         rd = (*scan >> 12) & 0xf;

         if ((*scan & 0x0f3f0000) == 0x051b0000) { // ldr[b] rX, [fp, #X]
            rdd = find_def(instInfo, &instInfo[(scan-funcBegin)+1], rd, S_FWD);
            rdu = find_use(instInfo, &instInfo[(scan-funcBegin)+1], rd, S_FWD);
            if (rdu != 0) {
               if (rdd == 0) rdd = &instInfo[funcEnd-funcBegin];
               rfinal = find_use_precede_def(instInfo, rdu, rdd, rd, S_FWD);
               for (rdt = &instInfo[(scan-funcBegin)+1];
                    rdt <= rfinal; ++rdt) if (*rdt & RI_bb) break;
            }
            if (rdu == 0 || rdt <= rfinal || // rdu == 0 means func ret value
                funcBegin[rdu-instInfo] == 0xe3500000) { // switch stmt
               *scan = 0xe1a00000 | (*scan & 0x0000f000) | (BR+i);
            }
            else {
               *scan = NOP;

               do {
                  int *rscan = &funcBegin[rdu-instInfo];
                  if ((*rscan & 0x0f3f0000) == 0x050b0000) { // frame var store
                     int off2 = (*rscan & 0xfff);
                     if ((*rscan & (1<<23)) == 0) off2 = -off2;
                     for (j = 0; j < numReg; ++j) {
                        if (offset[j] == off2) break;
                     }
                     if (j != numReg) {
                        if (i == j)
                           *rscan = NOP;
                        else
                           *rscan = 0xe1a00000 | ((BR+j) << 12) | (BR+i);
                        goto nextUse;
                     }
                  }
                  reg_rename(BR+i, rd, rdu, rscan);
nextUse:
                  if (rdu == rfinal) break;
                  rdu = find_use(instInfo, &instInfo[(rscan-funcBegin)+1], rd, S_FWD);
               } while (1);
            }
         }
         else { // str[b] rX, [fp, #X]
            *scan = 0xe1a00000 | ((BR+i) << 12) | ((*scan >> 12) & 0xf);
         }
      }
   }

   /* load frame vars into registers at top of function */
   for (scan = funcBegin; *scan != NOP; ++scan);
   for (i = 0; i < numReg; ++i) {
      *scan++ = 0xe51b0000 | ((BR + i) << 12) |
                ((offset[i] < 0) ? -offset[i] : (offset[i] | (1<<23)));
   }
}


/**********************************************************/
/********* Peephole optimization driver function **********/
/**********************************************************/

int squint_opt(int *begin, int *end)
{
   int optApplied = 0 ;
   int *scan = begin;
   int *tmpbuf = (int *) malloc((end-begin+1)*sizeof(int));

   create_const_map(begin, end);
   const_imm_opt(begin, end);

   while (scan < end) {
      if (*scan == 0xe92d4800 && !is_const(scan)) { // push {fp, lr}
         int *funcBegin = scan;
         int *funcEnd;
         int *retAddr = 0;
         ++scan;
         while (scan < end) {
            if (*scan == 0xe92d4800 && !is_const(scan)) { // push {fp, lr}
               break;
            }
            else if (*scan == 0xe8bd8800 && !is_const(scan)) { // pop {fp, pc}
               retAddr = scan;
            }
            ++scan;
         }
         --scan;
         funcEnd = scan /* retAddr */;

         // verify this function has been prepared for peephole opt
         if (funcBegin[3] != NOP) continue;

         /******************************************/
         /***   convert stack VM to frame VM     ***/
         /******************************************/

         simplify_branch(funcBegin, retAddr);
         apply_peepholes1(funcBegin, retAddr);
         apply_peepholes2(tmpbuf, funcBegin, retAddr);
         apply_peepholes1(funcBegin, retAddr);
         apply_peepholes3(tmpbuf, funcBegin, retAddr);
         create_pushpop_map(tmpbuf, funcBegin, retAddr);
         apply_peepholes4(funcBegin, retAddr);

         /******************************************/
         /***  convert frame VM to register VM   ***/
         /******************************************/

         rename_register1(tmpbuf, funcBegin, retAddr);
         apply_peepholes5(tmpbuf, funcBegin, retAddr);
         apply_peepholes6(tmpbuf, funcBegin, retAddr);

         rename_nop(funcBegin, retAddr);

         apply_peepholes7(tmpbuf, funcBegin, retAddr);
         simplify_frame(funcBegin, retAddr);

         funcEnd = relocate_nop(funcBegin, funcEnd, 0);
         optApplied = 1;
      }
      else {
         ++scan;
      }
   }
   destroy_const_map();
   free(tmpbuf);
   return optApplied;
}

#ifndef SQUINT_SO

/**********************************************************/
/******** read exe, optimize, write exe *******************/
/**********************************************************/

int main(int argc, char *argv[])
{
   int fd;
   int offset, length;
   int *mem;

   if (argc != 4) {
      printf("Use: %s <mc executable> <text start> <length>\n", argv[0]);
      exit(-1);
   }

   offset = strtol(argv[2], 0, 16);
   length = strtol(argv[3], 0, 16);

   if (!(offset > 0 && length > 0)) {
       printf("last two arguments must be hex file offset and length\n");
       exit(-1);
   }

   mem = (int *) malloc(length);

   if ((fd = open(argv[1], O_RDWR)) < 0) {
      printf("could not open file %s\n", argv[1]);
      exit(-1);
   }

   if (lseek(fd, (off_t) offset, SEEK_SET) != offset) {
      printf("could not seek to offset %x in file %s.\n", offset, argv[1]);
      exit(-1);
   }

   if (read(fd, mem, (size_t)length) != length) {
      printf("could not read %x bytes from file %s.\n", length, argv[1]);
      exit(-1);
   }

   if (!squint_opt(mem, mem + length/4)) {
      printf("Compile with mc -Op flag to enable peephole optimizer\n");
      close(fd);
      exit(0);
   }

   if (lseek(fd, (off_t) offset, SEEK_SET) != offset) {
      printf("could not seek to offset %x in file %s.\n", offset, argv[1]);
      exit(-1);
   }

   if (write(fd, mem, (size_t)length) != length) {
      printf("error occured attempting to write file %s\n", argv[1]);
      exit(-1);
   }

   close(fd);
   free(mem);

   printf("executable file %s was optimized.\n", argv[1]);

   return 0;
}

#endif
