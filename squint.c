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
#define RI_float   0x02000000 /* floating point op */
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
#define NOP11     0xe1a0b00b
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
   return low; // this is the inseertion point, which can be cnst_pool_size
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
           (*scan & 0xffbf0f00) == 0xed9f0a00)) {      // vldr s0, [pc, #X]
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

static void delete_const(int *cnst, int *scan)
{
   int addr = (scan-cbegin)*4;
   int i = find_const((cnst-cbegin)*4);
   struct ia_s *next, **inst = &cnst_pool[i].inst;
   while ((*inst)->inst_addr != addr) inst = &(*inst)->next;
   next = (*inst)->next;
   free(*inst);
   *inst = next;
   if (cnst_pool[i].inst == 0) *cnst = NOP;
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
            if ((*newinst & 0xffbf0f00) == 0xed9f0a00) goto skip_fp; // vldr
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

static void pack_const(int start)
{
   int i, j;
   for (i = start, j = start; i < cnst_pool_size; ++i) {
      if (cnst_pool[i].inst != 0) {
         if (i != j) {
            cnst_pool[j].data_addr = cnst_pool[i].data_addr;
            cnst_pool[j].inst      = cnst_pool[i].inst;
         }
         ++j;
      }
   }
   cnst_pool_size = j;

   return;
}

/* relocate a (v)ldr rX, [pc, #X] instruction */
static void rel_pc_ldr(int *dst, int *src)
{
   if (dst == src) return;

   int is_vldr = ((*src & 0xffbf0f00) == 0xed9f0a00); // vldr
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

/**********************************************************/
/************* NOP related utility functions **************/
/**********************************************************/

/* check for nop, PHD, and PHR0 instructions */
static int is_nop(int inst) {
   return ( (inst == NOP) || (inst == NOP1) ||
            (inst == NOP11) || (inst == NOP13) );
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
      if (*scan == NOP || *scan == NOP11 || *scan == NOP13) {
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
         if ((inst & 0xff200f00) == 0xed000a00) // vldr | vstr
            info |= RI_RnAct;
         if ((inst & 0xfff00f10) == 0xec500b10) // vmov rx, ry, dz
            info |= RI_RnAct | RI_RdAct | RI_RnDest | RI_RdDest;
      }
      else if (instMask == 0x0e) { /* float */
         info |= RI_float;
         if (((inst>>21) & 7) == 0 && // vmov sz, rx | (bit 20) vmov rx, sz
              (inst & 0x10) == 0x10) {
            info |= RI_RdAct | ((inst & (1<<20)) ? (RI_RdDest) : 0);
         }
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

/* Floating point (hack) version of create_inst_info */
static void create_inst_info_f(int *instInfo, int *funcBegin, int *funcEnd)
{
   int *scan;
   int *rInfo = instInfo;

   for (scan = funcBegin; scan <= funcEnd; ++scan) {

      /* skip code that won't be transformed */
      if (is_nop(*scan)) {
         int *end = skip_nop(scan, S_FWD);
         while (scan < end) {
            if (*scan == NOP13) {
               *rInfo++ = RI_func; // R0 set in func
            }
            else
               *rInfo++ = 0; // not FP inst
            ++scan;
         }
      }

      if (*scan == 0xecfd0a01) // vpop
         *rInfo = RI_RdAct | RI_RdDest | 0x1000;
      else if (*scan == 0xed2d0a01) // vpush
         *rInfo = RI_RdAct;
      else if ((*scan & 0xff300f00) == 0xed100a00) // vldr
         *rInfo = RI_RdAct | RI_RdDest | ((*scan & RI_Rd) * 2) |
                  ((*scan & 0x400000) ? 0x1000 : 0);
      else if ((*scan & 0xff300f00) == 0xed000a00) // vstr s0
         *rInfo = RI_RdAct | ((*scan & RI_Rd) * 2) |
                  ((*scan & 0x400000) ? 0x1000 : 0);
      else if ((*scan & 0xffbf0fd0) == 0xeeb00a40) // vmov Fd, Fm
         *rInfo = RI_RdAct | RI_RdDest | RI_RmAct |
                  ((*scan & 0x400000) ? 0x1000 : 0) | ((*scan & RI_Rd) * 2) |
                  ((*scan & 0x20) ? 1 : 0) | ((*scan & RI_Rm) * 2);
      else if (*scan == 0xee300a40) // vsub s0, s0, s0
         *rInfo = RI_RdAct | RI_RdDest; // def of s0, but not a use
      else if ((*scan & 0xffbf0fff) == 0xeeb50ac0) // vcmpe Fd1, #0
         *rInfo = RI_RdAct | ((*scan & RI_Rd) * 2) | ((*scan & (1<<22)) >> 10);
      else if (*scan == 0xeef40ac0) // vcmpe Fd, Fm0
         *rInfo = RI_RdAct | RI_RmAct |
                             ((*scan & RI_Rd) * 2) | ((*scan & (1<<22)) >> 10);
      else if ((*scan & 0xffbf0fc0) == 0xeeb10ac0) // vsqrt Fd, Fm
         *rInfo = RI_RdAct | RI_RdDest | RI_RmAct |
                    ((*scan & 0x400000) ? 0x1000 : 0) | ((*scan & RI_Rd) * 2) |
                    ((*scan & 0x20) ? 1 : 0) | ((*scan & RI_Rm) * 2);
      else if ((*scan & 0xff000f10) == 0xee000a00) // Fop Fd, Fn, Fm
         *rInfo = RI_RdAct | RI_RdDest | RI_RnAct | RI_RmAct |
                    ((*scan & 0x400000) ? 0x1000 : 0) | ((*scan & RI_Rd) * 2) |
                    ((*scan & 0x80) ? 0x10000 : 0) | ((*scan & RI_Rn) * 2) |
                    ((*scan & 0x20) ? 1 : 0) | ((*scan & RI_Rm) * 2);
      else if ((*scan & 0x0e000000) == 0x0a000000)
         *rInfo = RI_branch;
      else
         *rInfo = 0;

      if ((*scan & 0xf0000000) < 0xe0000000) {
         *rInfo |= RI_cond; /* conditionally executed inst */
      }

      ++rInfo;
   }

   /* termination sentinel to simplify reg_info scans */
   *rInfo = 0xffffffff;
}

/* Mark block boundaries (after jump inst, or branch target inst) */
static void create_bb_info(int *instInfo, int *funcBegin, int *funcEnd)
{
   int *scan;
   int *rInfo = instInfo;
   int *dst;
   int off;

   for (scan = funcBegin; scan <= funcEnd; ++scan) {
      if ((*rInfo & RI_branch) == RI_branch) {
         if ((*rInfo & RI_cond) == RI_cond) {
            dst = skip_nop(&funcBegin[rInfo-instInfo]+1, S_FWD);
            off = dst - funcBegin;
            instInfo[off] |= RI_bb;
            // rInfo[1] = rInfo[1] | RI_bb;
         }
         if (((*scan >> 24) & 0x0f) == 0x0a) { /* local branch */
            int tmp = (*scan & 0x00ffffff) |
                      ((*scan & 0x00800000) ? 0xff000000 : 0);
            dst = skip_nop(scan + 2 + tmp, S_FWD);
            off = dst - funcBegin;
            instInfo[off] |= RI_bb;
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

   if (mask & RI_Active) { // rename registers
      *inst = (*inst & ~activeRegMask[mask >> 4]) | regSet;
      *use  = (*use & ~activeRegMask[mask >> 4]) | regSet;
   }
}

static void reg_rename_f(int newreg, int oldreg, int *use, int *inst)
{
   int regSet = 0;
   int mask = (*use & RI_Active);

   if (*use & RI_hasD) // don't overwite dest reg
      mask &= ~RI_RdAct;

   if ((mask & RI_RmAct) && (*use & RI_Rm) == oldreg)
      regSet |= newreg;
   else
      mask &= ~RI_RmAct;

   if ((mask & RI_RdAct) && ((*use & RI_Rd) >> 12) == oldreg)
      regSet |= (newreg << 12);
   else
      mask &= ~RI_RdAct;

   if ((mask & RI_RnAct) && ((*use & RI_Rn) >> 16) == oldreg)
      regSet |= (newreg << 16);
   else
      mask &= ~RI_RnAct;

   if (mask & RI_Active) { // rename registers
      int tmp = *inst;
      if (mask & RI_RdAct) {
         tmp = (tmp & ~(RI_Rd | 0x00400000)) |
               ((newreg & 0x1e) << 11) | ((newreg & 1) ? (1<<22) : 0);
      }
      if (mask & RI_RnAct) {
         tmp = (tmp & ~(RI_Rn | 0x80)) |
               ((newreg & 0x1e) << 15) | ((newreg & 1) ? 0x80 : 0);
      }
      if (mask & RI_RmAct) {
         tmp = (tmp & ~(RI_Rm | 0x20)) |
               ((newreg & 0x1e)>>1) | ((newreg & 1) ? 0x20 : 0);
      }
      *inst = tmp;

      *use  = (*use & ~activeRegMask[mask >> 4]) | regSet;
   }
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
            if ((*scanp1 & 0xff300fff) == 0xe5100000 &&
                ((*scanp1 >> 16) & 0xf) == rI) { // ldr[b] rX, [rI]

               *scanp1 = (*scanp1 & 0xff70f000) | (*scan & 0xff) |
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
                  if ((*fscan  & 0xff70f000) == 0xe5100000 || // ldr r0, [xxx]
                      (*fscan  & 0xfffff000) == 0xe24b0000 || // sub r0, fp, #x
                      (*fscan == 0xe08b0000)) {               // add r0, fp, r0
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

static void apply_peepholes3_5(int *funcBegin, int *funcEnd)
{
   int *scan, *scanp1;

   for (scan = funcBegin; scan < funcEnd; ++scan) {
      scan = skip_nop(scan, S_FWD);
      scanp1 = active_inst(scan, 1);

      if ((*scan & 0xff000000) == 0xea000000) { // chk branch to next stmt
         int tmp = (*scan & 0xffffff) | ((*scan & 0x800000) ? 0xff000000 : 0);
         scanp1 = active_inst(scan,1);
         if ((scan + 2 + tmp) <= scanp1 && (scan + 2 + tmp) > scan) {
            // verify all NOPs between scan and scanp1
            while (--scanp1 != scan) {
               if (*scanp1 != NOP) break;
            }
            if (scan == scanp1) *scan = NOP;
         }
      }
      else if (*scan == 0xeef1fa10) {      // vmrs  APSR_nzcv, fpscr
         int *scanp3 = active_inst(scan, 3);
         if (*scanp3 == 0xe3500000) {      //  cmp   r0, #0
            int *scanp4 = active_inst(scanp3, 1);
            int *scanp6 = active_inst(scanp4, 2);
            if ((*scanp4 & 0x0f000000) == 0x0a000000 && // beq
                (*scanp6 & 0xff000000) == 0xea000000) { // b
               int *scanp5 = active_inst(scanp4, 1);
               int *scanp7 = active_inst(scanp6, 1);
               if ((*scanp5 & RI_Rd) == (*scanp7 & RI_Rd) &&
                   (*scanp5 & RI_Rd) == 0) {
                  int *scanp1 = active_inst(scan, 1);
                  *scanp6 = ((*scanp6 & 0x0fffffff) | (*scanp1 & 0xf0000000))
                               ^ (((*scanp1 & 0xff) == 0) ? 0x10000000 : 0);
                  *scanp1 = NOP; //scanp2
                  scanp1[1] = NOP;
                  *scanp3 = NOP;
                  *scanp4 = NOP;
               }
            }
         }
      }
   }
}

static void apply_peepholes4(int *funcBegin, int *funcEnd)
{
   int *scan, *scanp1;

   for (scan = funcBegin; scan < funcEnd; ++scan) {
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
         *scanp1 |= (1<<25) | (*scan & 0xff);
         *scan = NOP;
      }
   }
}

static void apply_peepholes4_5(int *instInfo, int *funcBegin, int *funcEnd)
{
   int *scan, *scanp1;

   create_inst_info_f(instInfo, funcBegin, funcEnd);
   create_bb_info(instInfo, funcBegin, funcEnd);

   for (scan = funcBegin; scan < funcEnd; ++scan) {
      scan = skip_nop(scan, S_FWD);

      if ((*scan & 0xfffff0d0) == 0xeeb00040) { // vmov s0, Fm
         scanp1 = active_inst(scan, 1);
         if ((*scanp1 & 0xffbf00ff) == 0xeeb00040 && // vmov Fn, s0
             (instInfo[scanp1-funcBegin] & RI_bb) == 0) {
            *scanp1 = *scanp1 | (*scan & 0x2f);
            *scan = NOP;
            scan = scanp1;
         }
      }
   }
}

static void apply_peepholes5(int *funcBegin, int *funcEnd)
{
   int *scan, *scanp1;

   for (scan = funcBegin; scan < funcEnd; ++scan) {
      scan = skip_nop(scan, S_FWD);
      scanp1 = active_inst(scan, 1);

      // after more optimization, do this before frame register allocation
      // in some cases, the vldr can be replaced with a register constant
      if ((  *scan & 0xff300f00) == 0xed100a00 && // vldr sN, [rx, #X]
          (*scanp1 & 0xff300f00) == 0xed000a00) { // vstr sN, [ry, #Y]
         // ldr r2, [rx, #X]; str r2, [ry, #Y];
          *scan = 0xe5102000 | (*scan & 0x8f0000) | ((*scan & 0xff) << 2);
        *scanp1 = 0xe5002000 | (*scanp1 & 0x8f0000) | ((*scanp1 & 0xff) << 2);
      }
      else if (*scan == 0xee300a40) {  // vsub.f32 s0, s0, s0
         if ((*scanp1 & 0xffbf0fff) == 0xeeb40ac0) {   // vcmpe.f32 sx, s0
            *scanp1 |= (1 << 16); *scan = NOP;
         }
      }
   }
}

static void apply_peepholes6(int *instInfo, int *funcBegin, int *funcEnd,
                             int dofloat)
{
   int *scan, *scanm1, *scanp1, *scanp2;
   int *info, *rxd, *rxu, *rdd, *rdu, *rdt, *rfinal;
   int rx, rd, rxS, rdm1, cond;
   int movMask = (dofloat) ? 0xffbf00d0 : 0xffff0ff0;
   int movInst = (dofloat) ? 0xeeb00040 : 0xe1a00000;

   if (dofloat)
      create_inst_info_f(instInfo, funcBegin, funcEnd);
   else
      create_inst_info(instInfo, funcBegin, funcEnd);

   create_bb_info(instInfo, funcBegin, funcEnd);

   // op rx, ...
   // mov rd, rx   --->  op rd, ...
   for (scan = funcBegin; scan < funcEnd; ++scan) {
      scan = skip_nop(scan, S_FWD);

      if ((*scan & movMask) == movInst) {
         if (instInfo[scan-funcBegin] & RI_bb) continue;
         rx = dofloat ?
              ((*scan & 0x0f)*2 + ((*scan & 0x20) ? 1 : 0)) : (*scan & 0x0f);
         rd = dofloat ?
              (((*scan & RI_Rd) >>11) + ((*scan & 0x400000) ? 1 : 0)) :
              ((*scan >> 12) & 0x0f);
         if (rd < (dofloat ? 2 : 3)) continue; // extra precaution
         scanm1 = active_inst(scan, -1);
         if ((*scanm1 & movMask) == movInst && // mov rx, ry
             (instInfo[scanm1-funcBegin] & RI_bb)) {
            continue;
         }
         // need dep info to check dest reg for mul...

         if (dofloat) {
            rdm1 = ((*scanm1 & RI_Rd) >> 11) + ((*scanm1 & 0x400000) ? 1 : 0 );
            cond = (rdm1 == rx);
         }
         else {
            rxS = (instInfo[scanm1-funcBegin] & RI_RdDest) ? 12 : 16;
            cond = (((*scanm1 >> rxS) & 0x0f) == rx &&
                    ((*scanm1 >> 25) & 7) <= 2);
         }

         if (cond) {
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
                     if (dofloat)
                        reg_rename_f(rd, rx, rxu, rscan);
                     else
                        reg_rename(rd, rx, rxu, rscan);
                     if (rxu == rfinal) break;
                     rxu = find_use(instInfo, &instInfo[rscan-funcBegin]+1,
                                    rx, S_FWD);
                  } while (1);
               }
            }
            if (rxu == 0 || rxu > rxd || rdt > rfinal) {
               if (dofloat)
                  *scanm1 = (*scanm1 & ~0x0040f000) | (*scan & 0x0040f000);
               else
                  *scanm1 = (*scanm1 & ~(0x0f << rxS)) | (rd << rxS);
               *scan = NOP;
            }
         }
      }
   }

   if (dofloat)
      create_inst_info_f(instInfo, funcBegin, funcEnd);
   else
      create_inst_info(instInfo, funcBegin, funcEnd);

   create_bb_info(instInfo, funcBegin, funcEnd);

   // mov rd, rx
   // op ..., rd   --->  op ..., rx
   for (scan = funcBegin; scan < funcEnd; ++scan) {
      scan = skip_nop(scan, S_FWD);

      if ((*scan & movMask) == movInst) {
         rx = dofloat ?
              ((*scan & 0x0f)*2 + ((*scan & 0x20) ? 1 : 0)) : (*scan & 0x0f);
         rd = dofloat ?
              (((*scan & RI_Rd) >>11) + ((*scan & 0x400000) ? 1 : 0)) :
              ((*scan >> 12) & 0x0f);
         scanp1 = active_inst(scan, 1);
         scanp2 = active_inst(scanp1, 1);
         info = &instInfo[scan-funcBegin];
         if ((*scanp1 == 0xe3500000 && // switch stmt -- cmp r0, #0; bne x
             (((*scanp2 >> 24) & 255) == 0x1a)) ||
             ((*scanp1 & movMask) == movInst && // mov rx, ry
              (*info & RI_bb)) ||
              rd > (dofloat ? 1 : 2)) {         // frame var assignment
             continue;
         }
         ++info;
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
                  if (dofloat)
                     reg_rename_f(rx, rd, rdu, rscan);
                  else
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
static void apply_peepholes7(int *instInfo, int *funcBegin, int *funcEnd)
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
            if (iscan > info && (inst & 0xfff00f00) == 0xe2800000) { // add
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


// struct member access
static void apply_peepholes7_5(int *instInfo, int *funcBegin, int *funcEnd)
{
   int *scan, *scanm, *scanmm, *scanmp, *info;
   int *rxd, *rxu, *rxuu, *rxdd, *rad, *rau, rx, opt;
   // int *rdt;

   create_inst_info(instInfo,funcBegin,funcEnd);
   create_bb_info(instInfo,funcBegin,funcEnd);

   for (scan = funcBegin; scan < funcEnd; ++scan) {
      scan = skip_nop(scan, S_FWD);

      if ((*scan & 0xffb0ffff) == 0xe5800000 ||  // str[b] r0, [rx]
          (*scan & 0xffb0ffff) == 0xed800a00) {  // vstr   s0, [rx]
         rx = (*scan & RI_Rn) >> 16;
         if (rx > 9) continue;
         info = &instInfo[scan-funcBegin];
         rxd = find_def(instInfo, info-1, rx, S_BACK);
         if (rxd == 0 || (*rxd & RI_func)) continue;
         scanm = &funcBegin[rxd-instInfo];
         if ((*scanm & 0xfff00f00) == 0xe2800000) { // add rx, ry, #N
            rxdd = find_def(instInfo, rxd-1, (*scanm & RI_Rn) >> 16, S_BACK);
            if (rxdd == 0 || (*rxdd & RI_func)) continue;
            rxu = find_use(instInfo, rxd-1, (*scanm & RI_Rn) >> 16, S_BACK);
            if (rxu == 0 || rxu <= rxdd) {
               rad = find_def(instInfo,rxd+1,(*scanm & RI_Rn) >> 16,S_FWD);
               if (rad < info) {
                  if (rad == 0 || (*rad & RI_func)) continue;
                  rau = find_use(instInfo,rad,(*scanm & RI_Rn) >> 16,S_BACK);
                  if (rau != rxd) continue;
               }
               scanmm = &funcBegin[rxdd-instInfo];
               opt = 0;
               if ((*scanmm & 0xffff0000) == 0xe24b0000) { // sub rN, fp, #X
                  opt = 2;
               }
               rxu = find_use(instInfo, rxd+1, rx, S_FWD);
               if (rxu != info) {
                  scanmp = &funcBegin[rxu-instInfo];
                  rxuu = find_use(instInfo, rxu+1, rx, S_FWD);
                  if (rxuu == info &&
                      ((*scanmp & 0xff300fff) == 0xe5100000 &&
                        ((*scanmp >> 16) & 0xf) == rx)) { // ldr[b] xx, [rX]
                     if (opt == 2) opt = 1;
                     else opt = 3;
                  }
               }
               else if (opt == 0) opt = 4;

               // for (rdt = rxdd+1; rdt <= info; ++rdt)
               //    if (*rdt & RI_bb) break;
               // if (rdt > info) {
                  switch(opt) { // disable case 1 and 2 to see structs in asm
                  case 1: // best performance
                     // move fp in ldr
                     *scanmp = ((*scanmp & 0xff70ff00) | (0xb << 16) |
                                (*scanmm & 0xff)) - (*scanm & 0xff);
                     if (((*scan >> 20) & 0xff) == 0xd8)
                        *scanmp = (*scanmp & 0xffffff00) |
                                  ((*scanmp & 0xff) >> 2);
                  case 2:
                     *scan = ((*scan & 0xff70ff00) | (0xb << 16) |
                              (*scanmm & 0xff)) - (*scanm & 0xff);
                     if (((*scan >> 20) & 0xff) == 0xd8)
                        *scan = (*scan & 0xffffff00) | ((*scan & 0xff) >> 2);
                     *scanmm = NOP;
                     *scanm  = NOP;
                     break;
                  case 3:
                     *scanmp = *scanmp | ((((*scan >> 20) & 0xff) != 0xd8) ?
                                (*scanm & 0xff) : ((*scanm & 0xff) >> 2));
                  case 4:
                     if (*rxdd & RI_RdDest)
                        *scanmm = (*scanmm & ~RI_Rd) | (rx << 12);
                     else // RI_RnDest
                        *scanmm = (*scanmm & ~RI_Rn) | (rx << 16);
                     *scan = *scan | ((((*scan >> 20) & 0xff) != 0xd8) ?
                             (*scanm & 0xff) : ((*scanm & 0xff) >> 2));
                     *scanm = NOP;
                  }
            // }
            }
         }
      }
   }
}


static void apply_peepholes8(int *instInfo, int *funcBegin, int *funcEnd,
                             int flow, int fhigh)
{
   int *scan, *scanm1, *scanp1;
   int *rdt, *info, *rfinal;
   int *rnu, *rnd, *rdu, *rdd, rn, rd, mask1, mask2;
   int *finfo = malloc((funcEnd-funcBegin+2)*sizeof(int));

   create_inst_info_f(finfo, funcBegin, funcEnd);
   create_bb_info(finfo, funcBegin, funcEnd);

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
            *scanp1 = 0xe1a00000 | (rd << 12); // mov rd, r0
            scan = scanp1;
         }
      }
      else if ((*scan & 0xfff00ff0) == 0xe0000090) { // mul rz, rx, ry
         scanp1 = active_inst(scan,1);
         if ((*scanp1 & 0xfff00ff0) == 0xe0800000) { // add rd, ra, rz
            *scanp1 = 0xe0200090 | ((*scanp1 & RI_Rd) << 4) |  // mla
            ((*scanp1 & RI_Rn) >> 4) | ((*scan & RI_Rd) >> 4) | (*scan & RI_Rm);
            *scan = NOP;
            scan = scanp1;
         }
      }
      else if ((*scan & 0xfff0ffff) == 0xe3500000) { // cmp rx, #0
         int rn = (*scan >> 16) & 0x0f;
         scanm1 = active_inst(scan,-1);
         if (((*scanm1 >> 12) & 0x0f) == rn) {
            int instMask  = *scanm1 & 0xfff00f00;
            int instMask2 = *scanm1 & 0xfff00ff0;
            if (instMask == 0xe2400000 || // sub rn, rx, #lit
                instMask == 0xe2800000 || // add rn, rx, #lit
                instMask == 0xe2000000 || // and rn, rx, #lit
                instMask2 == 0xe0400000 || // sub rn, rx, ry
                instMask2 == 0xe0800000 || // add rn, rx, ry
                instMask2 == 0xe0000000) { // and rn, rx, ry
               *scan = NOP;
               *scanm1 = *scanm1 | (1<<20);
            }
         }
      }
   }

   --funcEnd;
   for (scan = funcBegin; scan < funcEnd; ++scan) {
      scan = skip_nop(scan, S_FWD);

      if ((*scan & 0xfffff000) == 0xe50b0000) { // str r0, [fp, #X]
         scanp1 = active_inst(scan,1);
         if ((*scanp1 & 0xfffff000) == 0xe51b0000 && // ldr r0, [fp, #X]
             (*scan & 0xfff) == (*scanp1 & 0xfff)) {
            info = &instInfo[scan-funcBegin]+1;
            rfinal = &instInfo[scanp1-funcBegin];
            for (rdt = info; rdt <= rfinal; ++rdt) if (*rdt & RI_bb) break;
            if (rdt > rfinal) {
               *scanp1 = NOP;
               scan = scanp1;
            }
         }
         else if ((*scanp1 & 0xfffff000) == 0xe51b1000 && // ldr r1, [fp, #X]
             (*scan & 0xfff) == (*scanp1 & 0xfff)) {
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
      else if ((*scan & 0xffb00f50) == 0xee200a00) {  // vmul sd, sn, sm
         scanp1 = active_inst(scan,1);
         if ((*scanp1 & 0xffb00f50) == 0xee300a00 ||  // vadd sd, sn, sm
             (*scanp1 & 0xffb00f50) == 0xee300a40) {  // vsub sd, sn, sm
            if ((*scanp1 & 0xffb00f50) == 0xee300a00) {
               mask1 = 0xff40ff00; mask2 = 0xff800f00;
            }
            else {
               mask1 = 0xff40ff40; mask2 = 0xff800f40;
            }
            if ((*scanp1 & RI_Rn) == ((*scanp1 & RI_Rd) << 4) &&
                (((*scanp1 >> 15) & 0x80) ^ (*scanp1 & 0x80)) == 0) {
               *scanp1 = (*scanp1 & mask1) | (*scan & 0x000f00af);
               *scan = NOP;
            }
            else if (((*scan & RI_Rd)>>16) == (*scanp1 & RI_Rm) &&
                     ((*scan & 0x00400000)>>17) == (*scanp1 & 0x20)) {
               rn = ((*scanp1 & RI_Rn)>>15) + ((*scanp1 & 0x80) ? 1 : 0);
               if (rn < flow || rn >= fhigh) continue;
               rnu = find_use(finfo, &finfo[scanp1-funcBegin+1], rn, S_FWD);
               if (rnu != 0) {
                  rnd = find_def(finfo, &finfo[scanp1-funcBegin+1], rn, S_FWD);
                  if (rnd == 0 || rnu <= rnd) continue;
               }
               rd = ((*scan & RI_Rd)>>11) + ((*scan & 0x00400000) ? 1 : 0);
               rdu = &finfo[scanp1-funcBegin];
               rdd = find_def(finfo, &finfo[scanp1-funcBegin+1], rd, S_FWD);
               if (rdd == 0) rdd = &finfo[funcEnd-funcBegin];
               rdd = find_use(finfo, rdd, rd, S_BACK); // last xform
               for (rdt = rdu+1; rdt<=rdd; ++rdt)
                  if (*rdt & RI_bb) break;
               if (rdt > rdd) {
                  *scanp1 = (*scanp1 & mask2) | ((*scanp1 & RI_Rn)>>4) |
                            ((*scanp1 & 0x80)<<15) | (*scan & 0x000f00af);
                  *scan = NOP;
                  do {
                     rdu = find_use(finfo, rdu+1, rd, S_FWD);
                     reg_rename_f(rn, rd,
                                  rdu, &funcBegin[rdu-finfo]);
                  } while (rdu < rdd);
               }
            }
         }
      }
   }
   ++funcEnd;

   free(finfo);
}

static void apply_ptr_cleanup(int *instInfo, int *funcBegin, int *funcEnd)
{
   int *scan, *scanp1;
   int *rdt, *info, *rfinal;

   create_inst_info(instInfo, funcBegin, funcEnd);
   create_bb_info(instInfo, funcBegin, funcEnd);

   for (scan = funcBegin; scan < funcEnd; ++scan) {
      scan = skip_nop(scan, S_FWD);

      if ((*scan & 0xfff00ff0) == 0xe0800100) { // add rd, rn, rm, lsl #2
         info = &instInfo[scan-funcBegin];
         rfinal = find_def(instInfo, info + 1,
                           ((*info & RI_Rd) >> 12), S_FWD);
         if (rfinal) {
            scanp1 = &funcBegin[rfinal-instInfo];
            if (*scan == *scanp1) {
               // clean out obvious duplicate pointer calculations
               if ((*scan & 0xf0000) > 0xa0000) continue;
               if (*info & RI_RnAct) {
                  if (((*info & RI_Rn) >> 4) != (*info & RI_Rd)) {
                     rdt = find_def(instInfo, info+1,
                                 ((*info & RI_Rn) >> 16), S_FWD);
                     if (rdt != 0 && rdt < rfinal) continue;
                  }
               }
               if (*info & RI_RmAct) {
                  if (((*info & RI_Rm) << 12) != (*info & RI_Rd)) {
                     rdt = find_def(instInfo, info+1,
                                 (*info & RI_Rm), S_FWD);
                     if (rdt != 0 && rdt < rfinal) continue;
                  }
               }
               for (rdt = info + 1; rdt <= rfinal; ++rdt)
                  if (*rdt & RI_bb) break;

               if (rdt > rfinal) *scanp1 = NOP;
            }
         }
      }
   }
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
               *scan ^= 1; // allows other optimizations, cf xx_branch5()
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

   int *scan, *packed;

   struct pd_s *cremap;
   struct ia_s **inst;
   int lowc, highc;
   int cremap_size = 0;

   // Relocate instruction stream consts
   do {
      done = 1;
      lowc  = find_const((funcBegin-cbegin)*4);
      highc = find_const((funcEnd+1-cbegin)*4); // past end of func

      for (ii=lowc; ii<highc; ++ii) {
         if (cnst_pool[ii].inst == 0) {
            pack_const(ii);
            done = 0;
         }
      }
   } while (!done);

   if (highc != lowc) {
      if (highc == cnst_pool_size ||
          cnst_pool[highc].data_addr > (funcEnd-cbegin)*4) {
         --highc;
      }
      cremap_size = highc - lowc + 1;
      cremap = calloc(cremap_size, sizeof(struct pd_s));
   }

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
            if (scan != packed) {
               tmp = find_const((scan-cbegin)*4);
               cremap[tmp-lowc].data_addr = (packed-cbegin)*4;
               *packed++ = *scan;
            }
         }
         else if (*scan != NOP) {
            if ((*scan & 0xffff0000) == 0xe59f0000 || // ldr  rN, [pc, #X]
                (*scan & 0xffbf0f00) == 0xed9f0a00) { // vldr sN, [pc, #X]
               if (scan != packed) {
                  int is_vldr = ((*scan & 0xffbf0f00) == 0xed9f0a00); // vldr
                  int offset = is_vldr ? (*scan & 0xff) : ((*scan & 0xfff) / 4);
                  tmp = find_const(((scan + 2 + offset) - cbegin)*4);

                  for (inst = &cremap[tmp-lowc].inst;
                       *inst != 0; inst = &(*inst)->next);
                  *inst = malloc(sizeof(struct ia_s));
                  (*inst)->inst_addr = (packed-cbegin)*4;
                  (*inst)->next = 0;
                  *packed++ = *scan;
               }
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

      // update const_pool load operations
      if (cremap_size) {
         for (ii=0; ii < cremap_size; ++ii) {
            tmp = cremap[ii].data_addr / 4;
            for (inst = &cremap[ii].inst; *inst != 0; inst = &(*inst)->next) {
               scan = &cbegin[(*inst)->inst_addr/4];
               if ((*scan & 0xffbf0f00) == 0xed9f0a00) { // vldr
                  *scan = (*scan & 0xffffff00) |  (&cbegin[tmp] - scan - 2);
               }
               else {
                  *scan = (*scan & 0xfffff000) | ((&cbegin[tmp] - scan - 2)*4);
               }
            }
            // need to free inst ptrs (struct ia_s memory)
         }
         free(cremap);
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
   int *scanm1, *scanm2, *scanp1;
   int *stack[10];
   int *scan;
   int np = 0;

   create_inst_info(instInfo, funcBegin, funcEnd);
   for (scan = funcBegin; scan < funcEnd; ++scan) {
      scan = skip_nop(scan, S_FWD);

      if ( *scan == 0xe52d0004 && // push {r0}
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

   if (lev != 0) {
      printf("bug: optimizer push/pop lev = %d\n", lev);
      exit(-1);
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
            // default case: pushed reg not used before pop
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
         else if (r0d < r0u &&
                 ((*scanm1 & 0xffffff00) == 0xe2800000)) { // add r0, r0, #X
            scanm2 = active_inst(scanm1,-1);
            if (*scanm2 == 0xe5900000) {  // ldr r0, [r0]
               int *scan2 = pair[i].push + 1;
               for (; scan2 < pair[i].pop; ++scan2) {
                  if (*scan2 == NOP13 && !is_const(scan2)) break;
               }
               if (scan2 != pair[i].pop) continue;
               r0d = find_def(instInfo, pushp1, 2, S_FWD);
               r0u = find_use(instInfo, pushp1, 2, S_FWD);
               if ((r0u == 0 || r0u > pair[i].pop) &&
                   (r0d == 0 || r0d > pair[i].pop)) {
                  *scanm2 = *scanm2 | (2 << 12);
                  *scanp1 = (*scanp1 & 0xff70ff00) | (1 << 23) |
                       0x00020000 | (*scanm1 & 0xff);
                  *scanm1 = NOP;
                  *pair[i].push = NOP;
                  *pair[i].pop  = NOP;
               }
            }
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

   if (lev != 0) {
      printf("bug: optimizer push/pop lev = %d\n", lev);
      exit(-1);
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

static void create_pushpop_map2(int *instInfo, int *funcBegin, int *funcEnd)
{
   int i, lev = 0;
   int maxlev = 0;
   int *scanm1;
   int *stack[10];
   int *scan;
   int np = 0;

   create_inst_info_f(instInfo, funcBegin, funcEnd);

   for (scan = funcBegin; scan < funcEnd; ++scan) {
      scan = skip_nop(scan, S_FWD);

      if (*scan == 0xed2d0a01 && // vpush {s0}
          *(scan-1) != NOP1 &&
          *(scan+3) != 0xe3a0780f &&
          *(scan+6) != 0xe3a0780f) {
         stack[lev] = scan;
         ++lev;
      }
      else if (*scan == 0xecfd0a01 && // vpop {s1}
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

   if (lev != 0) {
      printf("bug: optimizer push/pop lev = %d\n", lev);
      exit(-1);
   }

   for (i = 0; i < np; ++i) {
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
            *scanm1 |= 0x400000; // Fd = 1
            *m1 |= 0x1000;
            *pair[i].push = NOP;
            *(--pushp1) = 0;
            *pair[i].pop  = NOP;
            *pop = 0;
         }
         else {
            *pair[i].push = 0xeef00a40; // vmov s1, s0
            *(--pushp1) = RI_RdAct | RI_RdDest | RI_RmAct | 0x1000;
            *pair[i].pop  = NOP;
            *pop = 0;
         }
      }
   }
}

static int create_pushpop_map3(int *instInfo, int *funcBegin, int *funcEnd,
                                int base, int dofloat)
{
   int *scan;
   int *scanm1, *scanp1;
   int i, guard, rd, rn;
   int *stack[10];
   int lev = 0;
   int np = 0;
   int maxreg = dofloat ? 16 : 11;
   int max_used = 0;

   if (dofloat)
      create_inst_info_f(instInfo, funcBegin, funcEnd);
   else
      create_inst_info(instInfo, funcBegin, funcEnd);

   create_bb_info(instInfo, funcBegin, funcEnd);

   for (scan = funcBegin; scan < funcEnd; ++scan) {
      scan = skip_nop(scan, S_FWD);

      guard = dofloat ? (*scan == 0xed2d0a01) : // vpush {s0}
              ((*scan & 0xffff0fff) == 0xe52d0004 && // push {r[0-9]}
               (*scan & 0xf000) < 0xa000);

      if (guard &&
          *(scan-1) != NOP1 &&
          *(scan+3) != 0xe3a0780f && // mov r7, #983040
          *(scan+6) != 0xe3a0780f) { // mc specific hack
         stack[lev] = scan;
         ++lev;
      }
      else {
         guard = dofloat ? (*scan == 0xecfd0a01) : // vpop {s1}
                 ((*scan & 0xffff0fff) == 0xe49d0004 && // pop {r[0-9]}
                  (*scan & 0xf000) < 0xa000);


         if (guard && *(scan-1) != NOP1) {
            --lev;
            pair[np].push = stack[lev];
            pair[np].pop  = scan;
            pair[np].lev  = lev;
            if (++np == 2000) {
               printf("pushpop overflow\n");
               exit(-1);
            }
         }
      }
   }

   if (lev != 0) {
      printf("bug: optimizer push/pop lev = %d\n", lev);
      exit(-1);
   }

   // innermost to outermost push/pop pairs
   for (i = 0; i < np; ++i) {

#ifdef OLD_CLEANUP_XFORM
      if (*scanp1 == 0xe5810000 || *scanp1 == 0xe5c10000 || // str[b] r0, [r1]
          *scanp1 == 0xed810a00) { // vstr s0, [r1]
         if (r0d < r0u && (instInfo[pair[i].push-funcBegin] & RI_bb) == 0 &&
            (*scanm1 & 0xff7ff000) == 0xe51b0000) { // ldr r0, [fp, #X]
            *pair[i].pop = *scanm1 | (1<<12);
            *scanm1 = NOP;
            *pair[i].push = NOP;
         }
      }
#endif

      for (scan = pair[i].push + 1; scan < pair[i].pop; ++scan) {
         if (*scan == NOP13 && !is_const(scan)) break; // func call
      }
      if (scan != pair[i].pop) continue; // skip regions with func call

      scanm1 = active_inst(pair[i].push,-1);
      scanp1 = active_inst(pair[i].pop,  1);

      if (dofloat && (*scanm1 & 0xfff0f050) == 0xeeb00040) { // vmov s0, Fm
         rn = (*scanm1 & 0x0f)*2 + ((*scanm1 & 0x20) ? 1 : 0);
         if (rn >= 2 && rn < base) {
            *scanm1 = NOP;
            *pair[i].push = NOP;
            *pair[i].pop = NOP;
            instInfo[scanm1-funcBegin] &= ~RI_bb;
            instInfo[pair[i].push-funcBegin] &= ~RI_bb;
            instInfo[pair[i].pop-funcBegin]  &= ~RI_bb;
            reg_rename_f(rn, 1, &instInfo[scanp1-funcBegin], scanp1);
            continue;
         }
      }

      int info = instInfo[scanm1-funcBegin];
      if ((info & RI_hasD) == 0) continue;

      rd = dofloat ? ((info & RI_Rd) >> 12) :
           ((info & RI_RdDest) ?
            ((info & RI_Rd) >> 12) : ((info & RI_Rn) >> 16));

      if (rd != (dofloat ? 0 : ((*pair[i].push & RI_Rd) >> 12))) continue;

      if ((instInfo[pair[i].push-funcBegin] & RI_bb) ||
          (instInfo[scanp1-funcBegin] & RI_bb)) {
         continue;
      }

      rn = dofloat ? 1 : ((*pair[i].pop & RI_Rd) >> 12);

      int *rnd = find_def(instInfo, &instInfo[scanp1-funcBegin], rn, S_FWD);
      if (rnd == 0) rnd = &instInfo[funcEnd-funcBegin];
      int *rnu = find_use(instInfo, &instInfo[scanp1-funcBegin], rn, S_FWD);
      int *rfinal = find_use_precede_def(instInfo, rnu, rnd, rn, S_FWD);

      for (scan = &instInfo[scanp1-funcBegin]; scan <= rfinal; ++scan) {
         if (*scan & RI_bb) break;
      }
      if (scan <= rfinal) continue;

      int reg, *rscan;
      int *pushp1 = &instInfo[pair[i].push-funcBegin]+1;
      for (reg = base; reg < maxreg; ++reg) {

         if (reg <= rd) continue;

         int *rxd = find_def(instInfo, pushp1, reg, S_FWD);
         int *rxu = find_use(instInfo, pushp1, reg, S_FWD);
         if (rxd != 0 && rxd <  rfinal) continue;
         if (rxu != 0 && rxu <= rfinal) continue;

         int *rdu = find_use(instInfo, pushp1, rd, S_FWD);
         int *rdd = find_def(instInfo, pushp1, rd, S_FWD);
         if (rdd == 0) rdd = &instInfo[pair[i].pop-funcBegin];
         if (rdu <= rdd) {
            int *xfinal = find_use_precede_def(instInfo, rdu, rdd, rd, S_FWD);
            do {
               rscan = &funcBegin[rdu-instInfo];
               if (dofloat)
                  reg_rename_f(reg, rd, rdu, rscan);
               else
                  reg_rename(reg, rd, rdu, rscan);

               if (rdu == xfinal) break;
               rdu = find_use(instInfo, &instInfo[rscan-funcBegin]+1,rd,S_FWD);
            } while (1);
         }

         // bump up available register threshold
         if (reg > max_used) max_used = reg;

         if (dofloat) {
            *scanm1 = (*scanm1 & ~(RI_Rd | 0x00400000)) |
                      ((reg & 0x1e) << 11) | ((reg & 1) ? (1<<22) : 0);
            instInfo[scanm1-funcBegin] = (info & ~RI_Rd) | (reg<<12);
         }
         else {
            if (((*scanm1 & 0x0e0000f0) == 0x90)) {
               *scanm1 = (*scanm1 & ~RI_Rn) | (reg<<16);
               instInfo[scanm1-funcBegin] = (info & ~RI_Rn) | (reg<<16);
            }
            else {
               *scanm1 = (*scanm1 & ~RI_Rd) | (reg<<12);
               instInfo[scanm1-funcBegin] = (info & ~RI_Rd) | (reg<<12);
            }
         }

         do {
            rscan = &funcBegin[rnu-instInfo];
            if (dofloat)
               reg_rename_f(reg, rn, rnu, rscan);
            else
               reg_rename(reg, rn, rnu, rscan);

            if (rnu == rfinal) break;
            rnu = find_use(instInfo, &instInfo[rscan-funcBegin]+1, rn, S_FWD);
         } while (1);
         *pair[i].push = NOP;
         *pair[i].pop  = NOP;
         instInfo[pair[i].push-funcBegin] = 0;
         instInfo[pair[i].pop-funcBegin]  = 0;

         break;
      }
   }
   return (max_used == 0) ? base : (max_used + 1);
}

/* simple functions with no locals do not need a frame */
void simplify_frame(int *funcBegin, int *funcEnd)
{
   int *scan, *scanp1;

   if (funcBegin[1] == 0xe28db000 && // add  fp, sp, #0
       ((funcBegin[2] & 0xfffff000) != 0xe24dd000 ||
       funcBegin[2] == 0xe24dd000)) { // sub sp, sp, #0
      int fo = -4 ; // frame offset

      if (funcBegin[2] == 0xe24dd000) funcBegin[2] = NOP;

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
            // rotate field will be >= 4 if present
            fo -= (((*scan & 0xf00) == 0) ? (*scan & 0xff) :
                   ((*scan & 0xff) << (32 - ((*scan & 0xf00) >> 8)*2)));
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

#define REN_BUF 128

// hack to support nonmutable floating point I-stream constants
// will likely need full dependency analysis later
static int rename_register1(int *instInfo, int *funcBegin, int *funcEnd,
                            int fbase)
{
   int *scan, *scanp1, *scanfp;
   int fpcnst[REN_BUF];
   int count[REN_BUF];
   int i, j, done, tmp, numReg = 0;

   for (i=0; i<REN_BUF; ++i) count[i] = 0;

   /* Extend this to support globals that are read but never written */


   /* record frame variable in this context */
   for (scan = funcBegin; scan <= funcEnd; ++scan) {
      scan = skip_nop(scan, S_FWD);

      if ((*scan & 0xffbf0f00) == 0xed9f0a00) { // vldr sN, [pc, #x]
         tmp = *(scan + 2 + (*scan & 0xff));
         for (i = 0; i < numReg; ++i) {
            if (fpcnst[i] == tmp) break;
         }
         if (i == numReg) {
            fpcnst[numReg++] = tmp;
            if (numReg == REN_BUF) break;
         }
         ++count[i];
      }
      else if ((*scan & 0xfffff000) == 0xe59f0000) { // ldr r0, [pc, #X]
         scanfp = active_inst(scan, 1);
         if (*scanfp == 0xed900a00) { // vldr s0, [r0]
            tmp = *(scan + 2 + (*scan & 0xfff)/4); // ptr to global
            for (i = 0; i < numReg; ++i) {
               if (fpcnst[i] == tmp) break;
            }
            if (i == numReg) {
               fpcnst[numReg++] = tmp;
               if (numReg == REN_BUF) break;
            }
            count[i] = (count[i] + 1) | 0x40000000;
         }
      }
      else if ((*scan & 0xfffff000) == 0xe59f1000) { // ldr r1, [pc, #X]
         scanfp = active_inst(scan, 1);
         if (*scanfp == 0xed810a00) { // vstr s0, [r1]
            tmp = *(scan + 2 + (*scan & 0xfff)/4); // ptr to global
            for (i = 0; i < numReg; ++i) {
               if (fpcnst[i] == tmp) break;
            }
            if (i == numReg) {
               fpcnst[numReg++] = tmp;
               if (numReg == REN_BUF) break;
            }
            count[i] |= 0x80000000; // write disqualifies this as const
         }
      }
   }

   // later: discard constants only used once outside of loop

   if (numReg == 0) return fbase;

   // descending sort
   do {
      done = 1;
      for (i=0; i<numReg-1; ++i) {
         if (count[i] < count[i+1]) {
            tmp = count[i];
            count[i] = count[i+1];
            count[i+1] = tmp;
            tmp = fpcnst[i];
            fpcnst[i] = fpcnst[i+1];
            fpcnst[i+1] = tmp;
            done = 0;
         }
      }
   } while(!done);

   // discard non-const values
   for (i=0; i<numReg; ++i) {
      if (count[i] < 0) {
         numReg = i;
         break;
      }
   }

   // Up to six FP constants for now
   if (numReg > 6)
      numReg = 6;

   j = (funcBegin[2] == NOP) ? 0 : 1; // adjust for new NOP slot

   if (numReg > 0) {
      for (scan = funcBegin; *scan != NOP; ++scan);
      for (i=0; i<numReg; ++i) {
         if (*scan != NOP) {
            printf("out of register assignment space\n");
            exit(-1);
         }
         if (count[i] & 0x40000000) { // global const
            if (scan[1] != NOP) {
               printf("out of register assignment space\n");
               exit(-1);
            }
            // ldr r0, [pc, #X]
            *scan = 0xe51f0000 | (((scan + 2) - (funcBegin-(i+j)))*4);
            scan[1] = 0xed900a00 | (((fbase+i) & 0x0e)<<11) |
                      (((fbase+i) & 1)*0x400000); // vldr s(fbase), [r0]
            ++scan;
         }
         else { // local const
            *scan = 0xed1f0a00 | (((fbase+i) & 0x0e)<<11) | // vldr
                    (((fbase+i) & 1)*0x400000) |
                    ((scan + 2) - (funcBegin-(i+j)));
         }
         *(funcBegin - (i+1)) = fpcnst[i];
         ++scan;
      }

      // Note: Likely need block boundary check on some of these...

      for (scan = funcBegin; scan <= funcEnd; ++scan) {
         scan = skip_nop(scan, S_FWD);

         if ((*scan & 0xffbf0f00) == 0xed9f0a00) { // vldr sN, [pc, #x]
            tmp = *(scan + 2 + (*scan & 0xff));
            delete_const(scan + 2 + (*scan & 0xff), scan);
            for (i = 0; i < numReg; ++i)  {
               if (fpcnst[i] == tmp) {
                  // *scan = 0xeeb00a40 | (*scan & 0x0040f000) |
                  //         ((fbase+i) >> 1) | (((fbase+i) & 1)*0x20);
                  scanp1 = active_inst(scan, 1);
                  if ((*scanp1 & 0xff70ff00) == 0xed000a00) { // vstr s0, ...
                     *scanp1 |= (((fbase+i) & 0x0e)<<11) |
                                (((fbase+i) & 1)*0x400000);
                     *scan = NOP;
                  }
                  else {
                     if (*scanp1 == 0xed2d0a01) { // vpush s0
                        *scan = 0xeeb00a40 | (*scan & 0x0040f000) | // vmov
                                ((fbase+i) >> 1) | (((fbase+i) & 1)*0x20);
                     }
                     else if (*scanp1 == 0xecfd0a01) { // vpop s1
                        scanp1 = active_inst(scanp1, 1);

                        create_inst_info_f(instInfo, scanp1, scanp1);
                        reg_rename_f(fbase+i, 0, instInfo, scanp1);
                        *scan = NOP;
                     }
                     else if ((*scanp1 & 0xffffff00) == 0xe59f1000) {
                        // ldr r1, [pc, #x]
                        scanp1 = active_inst(scanp1, 1);
                        if ((*scanp1 & 0xffffff00) == 0xed810a00) {
                           // vstr s0, [r1, #x]
                           *scanp1 |= (((fbase+i) & 0x0e)<<11) |
                                      (((fbase+i) & 1)*0x400000);
                           *scan = NOP;
                        }
                        else {
                           printf("screwed!\n"); exit(-1);
                        }
                     }
                     else {
                        printf("screwed!\n"); exit(-1);
                     }
                  }
               }
            }
         }
         else if ((*scan & 0xfffff000) == 0xe59f0000) { // ldr r0, [pc, #X]
            scanfp = active_inst(scan, 1);
            if (*scanfp == 0xed900a00) { // vldr s0, [r0]
               tmp = *(scan + 2 + (*scan & 0xfff)/4);
               delete_const(scan + 2 + (*scan & 0xfff)/4, scan);
               for (i = 0; i < numReg; ++i)  {
                  if (fpcnst[i] == tmp) {
                     scanp1 = active_inst(scanfp, 1);
                     if ((*scanp1 & 0xff70ff00) == 0xed000a00) { // vstr s0, ...
                        *scanp1 |= (((fbase+i) & 0x0e)<<11) |
                                   (((fbase+i) & 1)*0x400000);
                        *scan = NOP;
                        *scanfp = NOP;
                     }
                     else {
                        if (*scanp1 == 0xed2d0a01) { // vpush s0
                           *scan = 0xeeb00a40 | (*scan & 0x0040f000) | // vmov
                                   ((fbase+i) >> 1) | (((fbase+i) & 1)*0x20);
                           *scanfp = NOP;
                        }
                        else if (*scanp1 == 0xecfd0a01) { // vpop s1
                           scanp1 = active_inst(scanp1, 1);
                           create_inst_info_f(instInfo, scanp1, scanp1);
                           reg_rename_f(fbase+i, 0, instInfo, scanp1);
                           *scan = NOP;
                           *scanfp = NOP;
                        }
                        else if ((*scanp1 & 0xffffff00) == 0xe59f1000) {
                           // ldr r1, [pc, #x]
                           scanp1 = active_inst(scanp1, 1);
                           if ((*scanp1 & 0xffffff00) == 0xed810a00) {
                              // vstr s0, [r1, #x]
                              *scanp1 |= (((fbase+i) & 0x0e)<<11) |
                                         (((fbase+i) & 1)*0x400000);
                              *scan = NOP;
                              *scanfp = NOP;
                           }
                           else {
                              printf("screwed!\n"); exit(-1);
                           }
                        }
                        else {
                           printf("screwed!\n"); exit(-1);
                        }
                     }
                  }
               }
            }
         }
      }
   }

   return (fbase + numReg);
}

static int rename_register2(int *instInfo, int *funcBegin, int *funcEnd,
                            int base, int dofloat)
{
   int *scan;
   int offset[REN_BUF];
   int count[REN_BUF];
   int i, j, numReg = 0;
   int memMask = dofloat ? 0xff2f0f00 : 0x0f2f0000;
   int memInst = dofloat ? 0xed0b0a00 : 0x050b0000;
   int lb = 1 << 20; // load bit
   int maxReg = dofloat ? 16 : 11;

   for (i=0; i<REN_BUF; ++i) count[i] = 0;

   /* record frame variable in this context */
   for (scan = funcBegin; scan <= funcEnd; ++scan) {
      scan = skip_nop(scan, S_FWD);

      if ((*scan & memMask) == memInst) { // load/store [fp, #X]
         int off = dofloat ? (*scan & 0xff) : (*scan & 0xfff);
         if  ((*scan & (1<<23)) == 0) off = -off;
         for (i = 0; i < numReg; ++i) {
            if (offset[i] == off) break;
         }
         if (i == numReg) {
            offset[numReg++] = off;
            if (numReg == REN_BUF) break;
         }
         ++count[i];
      }
   }

   /* discard any frame variables that are not trivial */
   for (scan = funcBegin; scan <= funcEnd; ++scan) {
      scan = skip_nop(scan, S_FWD);

      if ((*scan & 0xffffff00) == 0xe28b0000 || // add r0, fp, #X
          (*scan & 0xffffff00) == 0xe24b0000) { // sub r0, fp, #X
         int off = dofloat ? ((*scan & 0xff) / 4) : (*scan & 0xff);
         if ((*scan & 0xffffff00) == 0xe24b0000) {
            off = -off;
         }
         for (i = 0; i < numReg; ++i) {
            if (offset[i] == off) break;
         }
         if (i != numReg) {
            --numReg;
            for(; i < numReg; ++i) {
               offset[i] = offset[i+1];
               count[i] = count[i+1];
            }
         }
      }
   }

   // do not waste register on low count operation
   // revise count later to consider loops
   for (i=0; i<numReg; ++i) {
      if (count[i] < 2) {
         --numReg;
         for(; i < numReg; ++i) {
            offset[i] = offset[i+1];
            count[i] = count[i+1];
         }
      }
   }

   if (numReg == 0) return base;

   int done;
   do {
      done = 1;
      for (i=0; i<numReg-1; ++i) {
         if (count[i] < count[i+1]) {
            int tmp = count[i];
            count[i] = count[i+1];
            count[i+1] = tmp;
            tmp = offset[i];
            offset[i] = offset[i+1];
            offset[i+1] = tmp;
            done = 0;
         }
      }
   } while(!done);

   if (base + numReg > maxReg) numReg = maxReg - base;

   if (dofloat) // dependency info
      create_inst_info_f(instInfo, funcBegin, funcEnd);
   else
      create_inst_info(instInfo, funcBegin, funcEnd);

   create_bb_info(instInfo, funcBegin, funcEnd);

   /* create registers for frame vars */
   for (scan = funcBegin; scan <= funcEnd; ++scan) {
      scan = skip_nop(scan, S_FWD);

      if ((*scan & memMask) == memInst) { // load/store [fp, #X]
         int rd;
         int *rdu, *rdd, *rdt, *rfinal;
         int off = dofloat ? (*scan & 0xff) : (*scan & 0xfff);
         if  ((*scan & (1<<23)) == 0) off = -off;

         for (i = 0; i < numReg; ++i) {
            if (offset[i] == off) break;
         }
         if (i == numReg) continue; // this frame var not mapped

        rd = dofloat ? (((*scan & RI_Rd) >> 11) + ((*scan & (1<<22)) ? 1 : 0)):
                        ((*scan >> 12) & 0xf);

         if ((*scan & memMask) == memInst && (*scan & lb)) { // load [fp, #X]
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
               if (dofloat)
                  *scan = 0xeeb00a40 | (*scan & 0x0040f000) |
                         ((base+i) >> 1) | (((base+i) & 1) ? 0x20 : 0);
               else
                  *scan = 0xe1a00000 | (*scan & 0x0000f000) | (base+i);
            }
            else {
               *scan = NOP;

               do {
                  int *rscan = &funcBegin[rdu-instInfo];
                  if ((*rscan & memMask) == memInst && (*rscan & lb) == 0) {
                     // frame var store
                     int off2 = dofloat ? (*rscan & 0xff) : (*rscan & 0xfff);
                     if ((*rscan & (1<<23)) == 0) off2 = -off2;
                     for (j = 0; j < numReg; ++j) {
                        if (offset[j] == off2) break;
                     }
                     if (j != numReg) {
                        if (i == j)
                           *rscan = NOP;
                        else {
                           if (dofloat)
                              *rscan = 0xeeb00a40 | (((base+j)&0x0e)<<11) |
                                       (((base+j)&1) ? 0x400000 : 0) |
                                       ((base+i)>>1) | (((base+i)&1) ? 0x20:0);
                           else
                              *rscan = 0xe1a00000 | ((base+j)<<12) | (base+i);
                        }
                        goto nextUse;
                     }
                  }
                  if (dofloat)
                     reg_rename_f(base+i, rd, rdu, rscan);
                  else
                     reg_rename(base+i, rd, rdu, rscan);
nextUse:
                  if (rdu == rfinal) break;
                  rdu = find_use(instInfo, &instInfo[(rscan-funcBegin)+1],
                                 rd, S_FWD);
               } while (1);
            }
         }
         else { // store [fp, #X]
            if (dofloat)
               *scan = 0xeeb00a40 |
                       (((base+i)&0x0e)<<11) | (((base+i)&1) ? 0x400000 : 0) |
                       ((*scan & RI_Rd)>>12) | ((*scan & 0x400000) ? 0x20 : 0);
            else
               *scan = 0xe1a00000 | ((base+i) << 12) | ((*scan >> 12) & 0xf);
         }
      }
   }

   /* load int frame vars into registers at top of function */
   for (scan = funcBegin; *scan != NOP; ++scan);
   j = 0;
   for (i = 0; i < numReg; ++i) {  // ldr rn, [fp, #X]
      if (*scan != NOP) {
         printf("out of register assignment space\n");
         exit(-1);
      }
      if (offset[i] >= 0) {
         if (dofloat)
            *scan++ = 0xed9b0a00 |
                      (((base+i)&0x0e)<<11) | (((base+i) & 1) ? 0x400000 : 0) |
                      offset[i] | (1<<23);
         else
            *scan++ = 0xe51b0000 | ((base + i) << 12) | offset[i] | (1<<23);
      }
      else if ((funcBegin[2] & 0xffffff00) == 0xe24dd000) { // sub sp, sp, #X
        ++j;
      }
   }

   if (!dofloat) {
      // Enable simplify_frame() optimization.
      if ((funcBegin[2] & 0xffffff00) == 0xe24dd000 && // sub sp, sp, #X
          (funcBegin[2] & 0xfff) == j*4) {
         funcBegin[2] = funcBegin[2] & 0xfffff000;
      }
   }

   return (base + numReg);
}


/**********************************************************/
/********* Peephole optimization driver function **********/
/**********************************************************/

int squint_opt(int *begin, int *end)
{
   int optApplied = 0 ;
   int *scan = begin;
   int *tmpbuf = (int *) malloc((end-begin+2)*sizeof(int));
   int noFloatConst;

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
         funcEnd = scan;

         // verify this function has been prepared for peephole opt
         if (funcBegin[3] != NOP) continue;

         // registers available to be allocated at or above this value
         int ibase = 3;
         int fbase = 2, flow, fhigh;
         int hasFuncCall = 0;

         // check for function calls
         for (scan = funcBegin; scan <= funcEnd; ++scan) {
            if (*scan == NOP13 && !is_const(scan)) {
               hasFuncCall = 1;
               break;
            }
         }
         scan = funcEnd;

         /******************************************/
         /***   convert stack VM to frame VM     ***/
         /******************************************/

         simplify_branch(funcBegin, retAddr);
         apply_peepholes1(funcBegin, retAddr);
         apply_peepholes2(tmpbuf, funcBegin, retAddr);
         apply_peepholes1(funcBegin, retAddr);
         apply_peepholes3(tmpbuf, funcBegin, retAddr);
         create_pushpop_map(tmpbuf, funcBegin, retAddr);

         if (!hasFuncCall)
            fbase = rename_register1(tmpbuf, funcBegin, retAddr, fbase);

         apply_peepholes3_5(funcBegin, retAddr);
         apply_peepholes1(funcBegin, retAddr);

         noFloatConst = (fbase == 2);
         create_pushpop_map2(tmpbuf, funcBegin, retAddr);
         apply_peepholes4(funcBegin, retAddr);

         /******************************************/
         /***  convert frame VM to register VM   ***/
         /******************************************/

         if (!hasFuncCall)
            ibase = rename_register2(tmpbuf, funcBegin, retAddr, ibase, 0);

         if (!noFloatConst) // correction for rename_register1
            apply_peepholes4_5(tmpbuf, funcBegin, retAddr);

         flow = fbase;
         fbase = create_pushpop_map3(tmpbuf, funcBegin, retAddr, fbase, 1);
         fhigh = fbase;

         if (!hasFuncCall)
            fbase = rename_register2(tmpbuf, funcBegin, retAddr, fbase, 1);

         apply_peepholes5(funcBegin, retAddr);
         apply_peepholes6(tmpbuf, funcBegin, retAddr, 0);

         if (!noFloatConst)
            apply_peepholes6(tmpbuf, funcBegin, retAddr, 1);

         apply_peepholes7(tmpbuf, funcBegin, retAddr);
         ibase = create_pushpop_map3(tmpbuf, funcBegin, retAddr, ibase, 0);
         apply_peepholes7_5(tmpbuf, funcBegin, retAddr);
         apply_ptr_cleanup(tmpbuf, funcBegin, retAddr);

         rename_nop(funcBegin, retAddr);

         apply_peepholes8(tmpbuf, funcBegin, retAddr, flow, fhigh);

         if (noFloatConst)
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
