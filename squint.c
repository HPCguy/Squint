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

#ifdef __MC__
#define static
#endif

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
#define RI_memRW   0x08000000 /* Read(1)/Write(0) mask */
#define RI_memR    0x08000000 /* Mem read op */

#define RI_instR   0x08000000 /* Instruction mem read */
#define RI_dataW   0x10000000 /* Rd written to memory */
#define RI_dataR   0x18000000 /* Rd read  from memory */
#define RI_frameW  0x20000000 /* Rd written to frame var */
#define RI_frameR  0x28000000 /* Rd read  from frame var */
#define RI_push    0x30000000 /* Rd is pushed onto stack */
#define RI_pop     0x38000000 /* Rd is popped off of stack */

#define RI_bb      0x04000000 /* basic block boundary */
#define RI_branch  0x02000000 /* branch instruction */
#define RI_FPacc   0x01000000 /* FP accumulate */
#define RI_func    0x00800000 /* function call */
#define RI_Sd      0x00400000 /* high bit of fp reg id */
#define RI_Sn      0x00200000 /* high bit of fp reg id */
#define RI_Sm      0x00100000 /* high bit of fp reg id */

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

// allocable integer registers
#define UREG_MASK 0x000057ff

enum CPUregs { rSL = 10, rFP, rIP, rSP, rLR, rPC };

enum search_dir { S_BACK = -1, S_FWD = 1 };

enum relocateMode { IntraFunc = 0, InterFunc };

static int *cnst_bit;

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

static int skip_const_blk;
static int extra_opt = 1;

#ifndef __MC__
#define clz __builtin_clz
#endif

/**********************************************************/
/*************** general utility functions ****************/
/**********************************************************/

static int popcount32b(int ii)
{
   int i = ii - ((ii >> 1) & 0x55555555); // add pairs of bits
   i = (i & 0x33333333) + ((i >> 2) & 0x33333333); // quads
   i = (i + (i >> 4)) & 0x0F0F0F0F; // groups of 8
   return (i * 0x01010101) >> 24; // horizontal sum of bytes
}

int avail_reg(int base)
{
   int i;
   int mask = 1;
   for (i=0; i<32; ++i, mask <<= 1) if (base & mask) break;
   return (i == 32) ? -1 : i;
}

/**********************************************************/
/********** I-stream const pool utility functions  ********/
/**********************************************************/

/* The ARM processor allows use of pc-relative constants */

int arith_off(int inst)
{
   return ((inst & 0xf00) == 0) ? (inst & 0xff) :
          ((inst & 0xff) << (16 - ((inst >> 8) & 0x0f)) * 2);
}

int gen_arith_off(int val)
{
   int highBit = 32 - (clz(val) & 0x1e); // need an even number of bits
   int lowBit = (highBit <= 8) ? 0 : (highBit - 8);
   return (val & ~(0xff << lowBit)) ? -1 :
          ((((16 - (lowBit >> 1)) & 0xf) << 8) | ((val >> lowBit) & 0xff));
}

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
   int iaddr = (inst-cbegin);
   return (cnst_bit[iaddr/(sizeof(int)*8)] &
               (1 << (iaddr & ((sizeof(int)*8)-1))));
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
   cnst_pool = (struct pd_s *) calloc(end-begin, sizeof(struct pd_s));
   cnst_bit  = (int *) calloc(1, ((end-begin)/8) + 8);
   cnst_pool_size = 0;

   while (scan < end) {
      if (((i = (*scan & 0xffff0000) == 0xe59f0000) || // ldr  r0, [pc, #X]
           (*scan & 0xffbf0f00) == 0xed9f0a00) &&      // vldr s0, [pc, #X]
           !is_const(scan)) {
         int offset = i ? ((*scan & 0xfff) / 4) : (*scan & 0xff);
         int iaddr = ((scan + 2 + offset) - begin);
         int addr = iaddr*4;
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
            cnst_bit[iaddr/(sizeof(int)*8)] |=
               1 << (iaddr & ((sizeof(int)*8)-1));
         }
         /* keep track of pc-relative instructions that ref this constant */
         for (inst = &cnst_pool[low].inst; *inst != 0; inst = &(*inst)->next);
         *inst = (struct ia_s *) malloc(sizeof(struct ia_s));
         (*inst)->inst_addr = (scan-begin)*4;
         (*inst)->next = 0;
      }
      ++scan;
   }
}

static void delete_const(int *cnst, int *scan)
{
   int iaddr = (cnst-cbegin);
   int addr = (scan-cbegin)*4;
   int i = find_const(iaddr*4);
   struct ia_s *next, **inst = &cnst_pool[i].inst;
   while (*inst && (*inst)->inst_addr != addr) inst = &(*inst)->next;
   if (*inst) {
      next = (*inst)->next;
      (*inst)->next = 0;
      free(*inst);
      *inst = next;
   }
   if (cnst_pool[i].inst == 0) {
      *cnst = NOP;
      cnst_bit[iaddr/(sizeof(int)*8)] &=
               ~(1 << (iaddr & ((sizeof(int)*8)-1)));
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
   free(cnst_bit);
   free(cnst_pool);
}

/* convert special ARM consts to mov immediate */
static void const_imm_opt(int *begin, int *end)
{
   int i;

   for (i = 0; i < cnst_pool_size; ++i) {
      int iaddr = cnst_pool[i].data_addr/4;
      int val = cbegin[iaddr];
      int aoff = gen_arith_off(val);

      if (aoff != -1 || (-256 <= val && val < 0)) {
         struct ia_s *inst = cnst_pool[i].inst;
         while (inst != 0) {
            struct ia_s *next = inst->next;
            int *newinst = cbegin + inst->inst_addr/4;
            if ((*newinst & 0xffbf0f00) != 0xed9f0a00) { // vldr
               if (aoff != -1) {
                  *newinst = 0xe3a00000 | (*newinst & RI_Rd) | aoff; // mov
               }
               else {
                  *newinst = 0xe3e00000 | (*newinst & RI_Rd) | -(val+1); // mvn
               }
               delete_const(&cbegin[iaddr], newinst);
            }
            inst = next;
         }
      }
   }
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

   int is_vldr = ((*src & 0xffbf0f00) == 0xed9f0a00); // vldr sd, [pc, #x]
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
   int *scan2, *scan = begin;
   int *after_const_block, done;

   do {
      done = 1;

      /* skip past any consts in instruction stream */
      while (is_const(scan)) scan += dir;

      /* skip past any NOPS in instruction stream */
      if (*scan == NOP || *scan == NOP11 || *scan == NOP13) {
         scan += dir;
         done = 0;
      }
      else if (dir == S_FWD &&
               *scan == NOP1 &&
               (*(scan+1) & 0xffff0fff) != 0xe52d0004) { /* push */
         scan += 2;
         done = 0;
      }
      else if (*(scan-1) == NOP1 &&
               (*scan & 0xffff0fff) != 0xe52d0004 && /* push */
               dir == S_BACK) {
         scan -= 2;
         done = 0;
      }
      else if (*scan == NOP1) {
         scan += dir;
         done = 0;
      }
      else if (skip_const_blk && (*scan & 0xff800000) == 0xea000000) {
         // expensive check but const-blocks have blocked many opts
         after_const_block = scan + 2 + (*scan & 0x00ffffff);
         for (scan2 = scan + 1; scan2 < after_const_block; ++scan2)
            if (!is_const(scan2)) break;
         if (scan2 == after_const_block) {
            scan = ((dir == S_FWD) ? after_const_block : (scan - 1));
            done = 0;
         }
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
static int *active_inst(int *cursor_, int index)
{
   int dir;
   int count;
   int *cursor = cursor_;

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

/****************************************************/
/****************** use-def analysis ****************/
/****************************************************/

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

// check if inst modifies flag register
static int mflag(int inst)
{
   return ((inst & 0x0e100000) == 0x02100000 || (inst == 0xeef1fa10) ||
           ((inst & 0x0e100000) == 0x00100000 && (inst & 0x90) != 0x90));
}

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
   int inst, instMask, op;

   for (scan = funcBegin; scan <= funcEnd; ++scan) {
      int info = 0;

      if (is_const(scan)) {
         goto next_iter;
      }
      else if (is_nop(*scan)) {
         info = ((*scan == NOP13) ? RI_func : 0);
         goto next_iter;
      }

      /* determine info about current instruction */
      inst = *scan;
      instMask   = (inst >> 24) & 0x0e;
      Rn         = (inst >> 16) & 0x0f;
      Rd         = (inst >> 12) & 0x0f;
      Rs         = (inst >>  8) & 0x0f;
      Rm         =  inst        & 0x0f;

#if 0
      if (inst == 0xe28fe000) { // add lr, pc, #0
      }
#endif
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
            if (inst & (1<<21)) info |= RI_RdAct; // mla
         }
         else if (inst == 0xe1600010) { // clz
            info |= RI_RdAct | RI_RdDest | RI_RmAct;
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
         int MEM_mask = (inst >> 20) & 0xd5;
         // HACK -- changes were made here to simplify read-only
         // dependency analysis related to auto inc-dec ldr/str.
         // If an optimization tries to modify an auto inc-dec
         // instruction, it might not preserve the inc-dec, CAUSING A BUG.
         if ((inst & RI_Rn) == 0x000d0000) { // stack operation
            if (MEM_mask == 0x41) /* pop */
               info |= RI_RdAct | RI_RnAct | RI_RdDest | RI_pop;
            else if (MEM_mask == 0x50) /* push */
               info |= RI_RdAct | RI_RnAct | RI_push;
         }
         else if (MEM_mask == 0x51 || MEM_mask == 0x55 ||
                  MEM_mask == 0x41) { /* ldr || ldrb */
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
         else if (MEM_mask == 0x50 || MEM_mask == 0x54 ||
                  MEM_mask == 0x40) { /* str | strb */
            info |= RI_RdAct | RI_RnAct;
            if (Rn == 0x0b) {
               info |= RI_frameW;
            }
            else {
               info |= RI_dataW;
            }
         }
      }
      else if (instMask == 0x0a) { /* BRANCH (and link) */
         info |= RI_branch;
      }
      else if (instMask == 0x0c) { /* float */
         if ((inst & 0xff200f00) == 0xed000a00) { // vldr | vstr
            info |= RI_RnAct;
            if ((inst & 0xff300f00) == 0xed100a00) { // vldr
               info |= ((Rn == 0x0f) ? RI_instR :
                        ((Rn == 0x0b) ? RI_frameR : RI_dataR));
            }
         }
         if ((inst & 0xfff00f10) == 0xec500b10) // vmov rx, ry, dz
            info |= RI_RnAct | RI_RdAct | RI_RnDest | RI_RdDest;
      }
      else if (instMask == 0x0e) { /* float */
         if (((inst>>21) & 7) == 0 && // vmov sz, rx | (bit 20) vmov rx, sz
              (inst & 0x10) == 0x10) {
            info |= RI_RdAct | ((inst & (1<<20)) ? (RI_RdDest) : 0);
         }
         else if ((inst & 0xfff00f7f) == 0xee100a10)   // fmrs
            *rInfo = RI_RdAct | RI_RdDest;
         else if ((inst & 0xfff00f7f) == 0xee000a10)   // fmsr
            *rInfo = RI_RdAct;
      }
      else if ((inst & 0xfff0f0f0) == 0xe750f010) {  // smmul
         info |= RI_RnAct | RI_RsAct | RI_RmAct | RI_RnDest;
      }

      /* Mask out any registers outside of rename range */

      if ( ((1 << Rn) & ~UREG_MASK) && regtest(info, RI_RnAct) ) {
         info &= ~(RI_RnAct | RI_RnDest);
      }
      if ( ((1 << Rd) & ~UREG_MASK) && regtest(info, RI_RdAct) ) {
         info &= ~(RI_RdAct | RI_RdDest);
         if ((info & RI_memMask) == RI_push) { /* check for push or pop */
            info &= ~RI_memOp; /* clear push/pop operation */
         }
         /* Note that RI_mem is left set here, since we still
            need to handle pc relative addresses */
      }
      if ( ((1 << Rs) & ~UREG_MASK) && regtest(info, RI_RsAct) ) {
         info &= ~RI_RsAct;
      }
      if ( ((1 << Rm) & ~UREG_MASK) && regtest(info, RI_RmAct) ) {
         info &= ~RI_RmAct;
      }

      /* keep register Ids for active registers */
      info |= inst & activeRegMask[(info & RI_Active) >> 4];

next_iter:
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

      // order is important
      if (is_const(scan))
         *rInfo = 0;
      else if (is_nop(*scan)) {
         *rInfo = ((*scan == NOP13) ? RI_func : 0);
      }
      else if ((*scan & 0xffbf0fff) == 0xecbd0a01 || // vpop Fd
          (*scan & 0xfe100f00) == 0xec100a00)   // vldr
         *rInfo = RI_RdAct | RI_RdDest | ((*scan & 0x7000) * 2) |
                  ((*scan & RI_Sd) >> 10) | ((*scan & 0x8000) << 7);
      else if (*scan == 0xed2d0a01) // vpush s0
         *rInfo = RI_RdAct;
      else if ((*scan & 0xfe100f00) == 0xec000a00) // vstr
         *rInfo = RI_RdAct | ((*scan & 0x7000) * 2) |
                  ((*scan & RI_Sd) >> 10) | ((*scan & 0x8000) << 7);
      else if ((*scan & 0xffbf0fd0) == 0xeeb00a40) // vmov Fd, Fm
         *rInfo = RI_RdAct | RI_RdDest | RI_RmAct | ((*scan & RI_Sd) >> 10) |
                  ((*scan & 0x7000) * 2) | ((*scan & 0x8000) << 7) |
                  ((*scan & 0x20) >> 5) | ((*scan & 0x07) * 2) |
                  ((*scan & 0x08) << 17);
      else if (*scan == 0xee300a40) // vsub s0, s0, s0
         *rInfo = RI_RdAct | RI_RdDest; // def of s0, but not a use
      else if ((*scan & 0xffbf0fff) == 0xeeb50ac0) // vcmpe Fd, #0
         *rInfo = RI_RdAct | ((*scan & 0x7000) * 2) |
                  ((*scan & RI_Sd) >> 10) | ((*scan & 0x8000) << 7);
      else if ((*scan & 0xffbf0fd0) == 0xeeb40ac0) // vcmpe Fd, Fm
         *rInfo = RI_RdAct | RI_RmAct | ((*scan & 0x7000) * 2) |
                  ((*scan & RI_Sd) >> 10) | ((*scan & 0x8000) << 7) |
                  ((*scan & 0x20) >> 5) | ((*scan & 0x07) * 2) |
                  ((*scan & 0x08) << 17);
      else if ((*scan & 0xffbf0fc0) == 0xeeb10ac0 || // vsqrt Fd, Fm
               (*scan & 0xffbf0fc0) == 0xeeb10a40 || // vneg  Fd, Fm
               (*scan & 0xffbf0fc0) == 0xeeb00ac0)   // vabs  Fd, Fm
         *rInfo = RI_RdAct | RI_RdDest | RI_RmAct | ((*scan & RI_Sd) >> 10) |
                    ((*scan & 0x7000) * 2) | ((*scan & 0x8000) << 7) |
                    ((*scan & 0x20) >> 5) | ((*scan & 0x07) * 2) |
                    ((*scan & 0x08) << 17);
      else if ((*scan & 0xffff0fd0) == 0xeeb70ac0) *rInfo = 0 ; // ignore
      else if ((*scan & 0xfff00f7f) == 0xee100a10)   // fmrs
         *rInfo = RI_RnAct | ((*scan & 0x70000) * 2) |
                    ((*scan & 0x80000) << 2) | ((*scan & 0x80) << 9);
      else if ((*scan & 0xfff00f7f) == 0xee000a10)   // fmsr
         *rInfo = RI_RnAct | RI_RnDest | ((*scan & 0x70000) * 2) |
                    ((*scan & 0x80000) << 2) | ((*scan & 0x80) << 9);
      else if ((*scan & 0xffbf0fd0) == 0xeeb80ac0 || // fsitos
               (*scan & 0xffbf0f50) == 0xeebd0a40)   // ftosis
         *rInfo = RI_RdAct | RI_RdDest | RI_RmAct | ((*scan & 0x20) >> 5) |
                    ((*scan & 0x07) * 2) | ((*scan & 0x08) << 17) |
                    ((*scan & RI_Sd) >> 10) | ((*scan & 0x7000) * 2) |
                    ((*scan & 0x8000) << 7);
      else if ((*scan & 0xff000f10) == 0xee000a00) { // Fop Fd, Fn, Fm
         *rInfo = RI_RdAct | RI_RdDest | RI_RnAct | RI_RmAct |
                    ((*scan & RI_Sd) >> 10) | ((*scan & 0x7000) * 2) |
                    ((*scan & 0x8000) << 7) | ((*scan & 0x80) << 9) |
                    ((*scan & 0x70000) * 2) | ((*scan & 0x80000) << 2) |
                    ((*scan & 0x20) >> 5) | ((*scan & 0x07) * 2) |
                    ((*scan & 0x08) << 17);
         if ((*scan & 0x00b00000) == 0) *rInfo |= RI_FPacc;
      }
      else if ((*scan & 0x0e000000) == 0x0a000000)
         *rInfo = RI_branch;
      else
         *rInfo = 0;

      ++rInfo;
   }

   /* termination sentinel to simplify reg_info scans */
   *rInfo = 0xffffffff;
}

/* Mark block boundaries (after jump inst, or branch target inst) */
static void create_bb_info(int *instInfo, int *funcBegin, int *funcEnd)
{
   int *info, *scan, *dst;
   int *instEnd = &instInfo[funcEnd-funcBegin];

   for (info = instInfo; info <= instEnd; ++info) {
      if (*info & RI_branch) {
         scan = &funcBegin[info-instInfo];
         if (((*scan >> 24) & 0x0f) == 0x0a) { // not a pc load
            if (((*scan >> 28) & 0x0f) < 0x0e) { // conditional branch
               // mark fall-through instruction
               dst = skip_nop(scan+1, S_FWD);
               instInfo[dst-funcBegin] |= RI_bb;
            }
            // ignore const blocks, but mark other branch destinations.
            // Note: const blocks act as NOP instructions for dep analysis.
            if (scan == skip_nop(scan, S_FWD)) {
               // mark jump destination instruction
               int tmp = (*scan & 0x00ffffff) |
                         ((*scan & 0x00800000) ? 0xff000000 : 0);
               dst = skip_nop(scan + 2 + tmp, S_FWD);
               instInfo[dst-funcBegin] |= RI_bb;
            }
         }
      }
   }
}

/* find first def of reg in given direction */
static int *find_def(int *instInfo, int *rInfo_, int reg, enum search_dir dir)
{
   int *retVal = 0;
   int *rInfo = rInfo_;
   int info;

   while( (rInfo > instInfo) &&
          *rInfo != 0xffffffff ) { /* if in-bounds of func */
      info = *rInfo;
      if (info & RI_hasD) {
         int cmpReg = (info & RI_RnDest) ?
                      (((info & RI_Rn)>>16) | ((info & RI_Sn)>>17)) :
                      (((info & RI_Rd)>>12) | ((info & RI_Sd)>>18));
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
static int *find_use(int *instInfo, int *rInfo_, int reg, enum search_dir dir)
{
   int *retVal = 0;
   int *rInfo = rInfo_;
   int info;

   while( (rInfo > instInfo) &&
          *rInfo != 0xffffffff ) { /* if in-bounds of func */
      info = *rInfo;
      if (info & RI_hasD) {
         // if not a memory op or a memory read, disable dest reg
         if ((info & (RI_memOp | RI_FPacc)) == 0 || (info & RI_memRW)) {
            info ^= (info & RI_RnDest) ? RI_RnAct : RI_RdAct;
         }
      }
      if ((info & RI_RmAct) &&
          ((info & RI_Rm) | ((info & RI_Sm)>>16)) == reg) {
         retVal = rInfo;
         break;
      }
      if (info & RI_RsAct && ((info & RI_Rs)>>8) == reg) {
         retVal = rInfo;
         break;
      }
      if (info & RI_RdAct &&
         (((info & RI_Rd)>>12) | ((info & RI_Sd)>>18)) == reg) {
         retVal = rInfo;
         break;
      }
      if (info & RI_RnAct &&
         (((info & RI_Rn)>>16) | ((info & RI_Sn)>>17)) == reg) {
         retVal = rInfo;
         break;
      }
      rInfo += dir;
   }

   return retVal;
}

/* pass in valid use and def pointer */
static int *find_use_precede_def(int *instInfo, int *use_, int *def,
                                 int reg, enum search_dir dir)
{
   int *use = use_;
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

   if ((*use & RI_hasD) /* && !(*use & RI_FPacc) */) // don't overwite dest reg
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
      int tmp = activeRegMask[mask >> 4];
      *inst = (*inst & ~tmp) | regSet;
      *use  = (*use & ~tmp) | regSet;
   }
}

static void reg_rename_f(int newreg, int oldreg, int *use, int *inst)
{
   int regSet = 0;
   int mask = (*use & RI_Active);

   if (*use & RI_hasD) // don't overwite dest reg
      mask &= ~RI_RdAct;

   if ((mask & RI_RmAct) &&
       ((*use & RI_Rm) | ((*use & RI_Sm) >> 16)) == oldreg)
      regSet |= ((newreg & 0x0f) | ((newreg & 0x10) << 16));
   else
      mask &= ~RI_RmAct;

   if ((mask & RI_RdAct) &&
       (((*use & RI_Rd) >> 12) | ((*use & RI_Sd) >> 18)) == oldreg)
      regSet |= (((newreg & 0x0f) << 12) | ((newreg & 0x10) << 18));
   else
      mask &= ~RI_RdAct;

   if ((mask & RI_RnAct) &&
       (((*use & RI_Rn) >> 16) | ((*use & RI_Sn) >> 17)) == oldreg)
      regSet |= (((newreg & 0x0f) << 16) | ((newreg & 0x10) << 17));
   else
      mask &= ~RI_RnAct;

   if (mask & RI_Active) { // rename registers
      int tmp = *inst;
      if (mask & RI_RdAct) {
         tmp = (tmp & ~(RI_Rd | RI_Sd)) |
               ((newreg & 0x1e) << 11) | ((newreg & 1) << 22);
      }
      if (mask & RI_RnAct) {
         tmp = (tmp & ~(RI_Rn | 0x80)) |
               ((newreg & 0x1e) << 15) | ((newreg & 1) << 7);
      }
      if (mask & RI_RmAct) {
         tmp = (tmp & ~(RI_Rm | 0x20)) |
               ((newreg & 0x1e)>>1) | ((newreg & 1) << 5);
      }
      *inst = tmp;

      *use  = (*use & ~activeRegMask[mask >> 4]) | regSet;
   }
}


/**********************************************************/
/********** peephole optimization funcs *******************/
/**********************************************************/

// remove add/sub of #0. Consolidate imm offset load, DP imm.
static void apply_peepholes1(int *funcBegin, int *funcEnd)
{
   int *scan, *scanp1, *scanp2, *scanp3, moff, isAdd;
   for (scan = funcBegin; scan < funcEnd; ++scan) {
      if (*scan == 0xe2400000 || /* sub r0, r0, #0 */
          *scan == 0xe2800000) { /* add r0, r0, #0 */
         if (!is_const(scan)) *scan = NOP;
      }
      else if ((*scan & 0xfffff000) == 0xe3a00000 && // mov r0, #X
                scan[1] == 0xe04b0000) {             // sub r0, fp, r0
         if (is_const(scan)) continue;
         scan[1] = 0xe24b0000 | (*scan & 0xfff);     // sub r0, fp, #X
         *scan = NOP;
      }
      else if ((*scan & 0xfffff000) == 0xe24b0000 && // sub r0, fp, #X
               (scan[2] & 0xfffff000) == 0xe3a00000 && // mov r0, #x
               ((isAdd = (scan[4] == 0xe0810000)) ||   // add  r0, r1, r0
                scan[4] == 0xe0410000) &&  // sub  r0, r1, r0
               scan[1] == 0xe52d0004 && // push {r0}
               scan[3] == 0xe49d1004) { // pop  {r1}
         int aoff = gen_arith_off(arith_off(*scan) +
                                  arith_off(scan[2])*(isAdd ? -1 : 1));
         if (aoff != -1) {
            scan[4] = (*scan & 0xfffff000) | aoff;
            scan[3] = scan[2] = scan[1] = scan[0] = NOP;
            scan += 4; // setting this to 3 breaks something
         }
      }
      else if ((*scan & 0xfff00000) == 0xe2800000 || // add rI, rS, #X
               (*scan & 0xfff00000) == 0xe2400000) { // sub rI, rS, #X
         if (is_const(scan)) continue;
         scanp1 = active_inst(scan, 1);
         int rS = (*scan & RI_Rn) >> 16; // Source, RI_Rn
         int rI = (*scan & RI_Rd) >> 12; // Index,  RI_Rd
         if ((*scanp1 & 0xff300000) == 0xe5100000 && // ldr[b] rX, [rI]
             ((*scanp1 & RI_Rn) >> 16) == rI &&
              ((1 << rI) & UREG_MASK)) {
            moff = (*scanp1 & 0xfff) * ((*scanp1 & (1<<23)) ? 1 : -1);
            moff += arith_off(*scan) *
                    (((*scan & 0xfff00000) == 0xe2800000) ? 1 : -1);
            if (moff < -4095 || moff > 4095) continue;
            *scanp1 = (*scanp1 & 0xff70f000) |
                      ((moff >= 0) ? (moff | (1<<23)) : -moff) |
                      (rS << 16); // ldr[b] rX, [rS, #X]
            *scan = NOP;
            scan = scanp1;
         }
         else if ((*scanp1 & 0xff7fff00) == 0xed100a00) { // vldr s0,[r0,#X]
            moff = (*scanp1 & 0xff) * ((*scanp1 & (1<<23)) ? 1 : -1);
            moff += (arith_off(*scan) / 4) *
                    (((*scan & 0xfff00000) == 0xe2800000) ? 1 : -1);
            if (moff < -255 || moff > 255) continue;
            *scanp1 = (*scanp1 & 0xff70ff00) |
                      ((moff >= 0) ? (moff | (1<<23)) : -moff) |
                      (rS << 16); // vldr s0, [rS, #X]
            *scan = NOP;
            scan = scanp1;
         }
      }
      else {
         /* change register to immediate if possible */
         if (*scan != 0xe52d0004) continue; // push {r0}
         if (is_const(scan)) continue;
         scanp1 = active_inst(scan,   1);
         if ((*scanp1 & 0xfffff000) != 0xe3a00000) continue; // mov r0, #X
         scanp2 = active_inst(scanp1, 1);
         if (*scanp2 == 0xe49d1004) { // pop  {r1}
            scanp3 = active_inst(scanp2, 1);
            if ((*scanp3 & 0xfe1fffff) == 0xe0010000 &&
                (*scanp3>>23 & 0x3) != 0x2) { /* exclude comparisons */
               *scanp3 = (*scanp3 ^ 1<<16) | 1<<25 | (*scanp1 & 0xfff);
               *scan   = NOP;
               *scanp1 = NOP;
               *scanp2 = NOP;
               scan = scanp3 - 1;
            }
            else if (*scanp3 == 0xe1a00051 || /* asr r0, r1, r0 */
                     *scanp3 == 0xe1a00011) { /* lsl r0, r1, r0 */
               int shift = ((*scanp1 & 0xff) < 0x20) ? (*scanp1 & 0x1f) : 0x1f;
               *scanp3 = (*scanp3 & 0xffffffe0) | (shift << 7);
               *scan   = NOP;
               *scanp1 = NOP;
               *scanp2 = NOP;
               scan = scanp3 - 1;
            }
         }
      }
   }
}

// find arr[x] pattern and consolidate index address into a single instruction
static void apply_peepholes2(int *instInfo, int *funcBegin, int *funcEnd_)
{
   int *scan;
   int *scanp1, *scanp2, *scanp3, *scanp4, *scanp5;
   int *scanm1, *scanm2;
   int *funcEnd = funcEnd_;
   int depInited = 0;

   funcEnd -= 6;
   for (scan = funcBegin; scan < funcEnd; ++scan) {
      /* Convert array addressing to more compact form */
      if ((*scan & 0xffffff00) == 0xe3a00000) { /* mov r0, #X */
         if (is_const(scan)) continue;
         int imm = (*scan & 0xff);
         if ((imm & (imm-1)) != 0) continue; // power of 2 check
         scanm1 = active_inst(scan, -1);
         scanp1 = active_inst(scan,  1);
         if (*scanm1 == 0xe52d0004 && /* push {r0} */
             *scanp1 == 0xe49d1004) { /* pop  {r1} */
            int *loc;
            scanp2 = active_inst(scanp1,  1);
            if (*scanp2 != 0xe0000091) continue; // mul r0, r1, r0
            scanp3 = active_inst(scanp2,  1);
            if (*scanp3 != 0xe49d1004) continue; // pop {r1}
            scanp4 = active_inst(scanp3,  1);
            if (*scanp4 != 0xe0810000) continue; // add r0, r1, r0
            scanm2 = active_inst(scanm1, -1);
            if (depInited == 0) {
               create_inst_info(instInfo, funcBegin, funcEnd);
               depInited = 1;
            }
            loc = &instInfo[scanm2-funcBegin];
            if (find_def(instInfo, loc, 0, S_FWD) == loc) {
               int lev=1;
               int *pscan = scanm2;
               while (lev != 0 && pscan > funcBegin) {
                  if ((*pscan & RI_Rn) == 0x20000 || (*pscan & RI_Rm) == 2)
                     goto avoid_nest;
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
               // move on to "load" instruction
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
                     scan = scanp5;
                  }
                  else {
                     // add r0, r2, r0, lsl #X
                     *scanp4 = 0xe0820000 | (imm << 7);
                     scan = scanp4;
                  }
               }
               else
                  scan = active_inst(scanp4, 1);
            }
         }
      }
avoid_nest: ;
   }
   funcEnd += 6;
}

// get rid of unused postincrement-operator cruft, e.g. for(;; i++)
static void apply_peepholes3(int *instInfo, int *funcBegin, int *funcEnd_)
{
   int *scan;
   int info;
   int *funcEnd = funcEnd_;

   create_inst_info(instInfo, funcBegin, funcEnd);

   funcEnd -= 5;
   for (scan = funcBegin; scan < funcEnd; ++scan) {
      scan = skip_nop(scan, S_FWD);
      info = instInfo[scan-funcBegin];
      if (info & RI_hasD) {
         int *next, *nextDef, *nextUse;
         int destR =
            ((info & RI_RdDest) ? (info&RI_Rd)>>12 : (info & RI_Rn)>>16) & 0xf;
         if (destR < rFP) { // early optimize phase means rIP/rLR not in play
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
                  instInfo[scan-funcBegin] = 0;// &= RI_bb;
               }
               scan = next-1;
            }
         }
      }
   }
   funcEnd += 5;
}

// optimize negative imm offset load.  Optimize for vcmpe rx, #0.0.
static void apply_peepholes3_2(int *funcBegin, int *funcEnd_)
{
   int *scan, *scanm1, *scanp1, *scanp2, *scanp3, moff, isFP;
   int *funcEnd = funcEnd_;

   funcEnd -= 3;
   for (scan = funcBegin; scan < funcEnd; ++scan) {
      if ((*scan & 0xffffff00) == 0xe3e00000) { // mvn r0, #x
         if (is_const(scan)) continue;
         scanm1 = active_inst(scan,-1);
         scanp1 = active_inst(scan, 1);
         if (*scanm1 == 0xe52d0004 && *scanp1 == 0xe49d1004) { // psh r0 pop r1
            scanp2 = active_inst(scanp1, 1);
            scanp3 = active_inst(scanp2, 1);
            if (*scanp2 == 0xe0810000 &&                        // add r0,r1,r0
                ((isFP = (*scanp3 & 0xff2fff00) == 0xed000a00)  // vldr|str
                    || (*scanp3 & 0xff2ff000) == 0xe5000000)) { // (ldr|str)[b]
               moff = isFP ? ((*scanp3 & 0xff)*4) : (*scanp3 & 0xfff);
               moff = moff * ((*scanp3 & (1<<23)) ? 1 : -1) -
                     ((*scan & 0xff) + 1);
               if (isFP) {
                  moff /= 4;
                  if (moff > -256 && moff < 256) {
                     *scanp3 = (*scanp3 & 0xff7fff00) |
                               ((moff >= 0) ? (moff | (1<<23)) : -moff);
                     *scanm1 = *scan = *scanp1 = *scanp2 = NOP;
                  }
               }
               else {
                  if (moff > -4096 && moff < 4096) {
                     *scanp3 = (*scanp3 & 0xff7ff000) |
                               ((moff >= 0) ? (moff | (1<<23)) : -moff);
                     *scanm1 = *scan = *scanp1 = *scanp2 = NOP;
                  }
               }
            }
         }
      }
      else if (*scan == 0xeef40ac0) { // vcmpe.f32 s1, s0
         if (is_const(scan)) continue;
         // more compact code generation -- but is it faster?
         scanm1 = active_inst(scan,   -1);
         if (*scanm1 == 0xecfd0a01) { // vpop s1
            int *scanm2 = active_inst(scanm1, -1);
            int *scanm3 = active_inst(scanm2, -1);
            if (*scanm3 == 0xed2d0a01) { // psh s0 pop s1
               if ((*scanm2 & 0xff7fff00) == 0xed1f0a00) { // vldr s0, [pc, #X]
                  // moff = (*scanm2 & 0xff)*((*scanm2 & (1<<23)) ? 1 : -1) + 2;
                  moff = (*scanm2 & 0xff) + 2; // only forward at this point
                  if (*(scanm2 + moff) == 0) { // need vcmpe.f32 s0, #0
                     delete_const(scanm2 + moff, scanm2);
                     *scan = (*scan ^ (1 << 22)) | (1 << 16); // vcmpe.f32 s0, #0
                     *scanm1 = *scanm2 = *scanm3 = NOP;
                  }
               }
            }
         }
      }
   }
   funcEnd += 3;
}

// remove all branches to immediately following active instruction.
// simplify brain-dead "comparison code-block" emmitted by compiler.
static void apply_peepholes3_5(int *funcBegin, int *funcEnd)
{
   int *scan, *scanp1, *scanp2, *scanp3, *scanp4, *scanp5, *scanp6, *scanp7;

   for (scan = funcBegin; scan < funcEnd; ++scan) {
      if ((*scan & 0xff000000) == 0xea000000) { // chk branch to next stmt
         if (is_const(scan)) continue;
         int tmp = (*scan & 0xffffff) | ((*scan & 0x800000) ? 0xff000000 : 0);
         scanp1 = active_inst(scan,1);
         if ((scan + 2 + tmp) <= scanp1 && (scan + 2 + tmp) > scan) {
            // verify all NOPs between scan and scanp1
            int *tscan = scanp1;
            while (--tscan != scan) {
               if (*tscan != NOP) break;
            }
            if (scan == tscan) {
               *scan = NOP;
               scan = scanp1 - 1;
            }
         }
      }
   }

   for (scan = funcBegin; scan < funcEnd; ++scan) {
      if (*scan == 0xeef1fa10) {      // vmrs  APSR_nzcv, fpscr
         if (is_const(scan)) continue;
         scanp3 = active_inst(scan, 3);
         if (*scanp3 == 0xe3500000) {      //  cmp   r0, #0
            scanp4 = active_inst(scanp3, 1);
            if ((*scanp4 & 0x0f000000) == 0x0a000000) { // branch
               scanp6 = active_inst(scanp4, 2);
               if ((*scanp4 & 0xf0000000) == 0 &&          // beq
                   (*scanp6 & 0xff000000) == 0xea000000) { // b
                  scanp5 = active_inst(scanp4, 1);
                  scanp7 = active_inst(scanp6, 1);
                  if ((*scanp5 & RI_Rd) == (*scanp7 & RI_Rd) &&
                      (*scanp5 & RI_Rd) == 0) {
                     scanp1 = active_inst(scan, 1);
                     *scanp6= ((*scanp6 & 0x0fffffff) | (*scanp1 & 0xf0000000))
                                  ^ (((*scanp1 & 0xff) == 0) ? 0x10000000 : 0);
                     *scanp1 = NOP;
                     scanp1[1] = NOP; // scanp2
                     *scanp3 = NOP;
                     *scanp4 = NOP;
                     scan = scanp6;
                  }
               }
               else {
                  int btype =
                     ((*scanp4 & 0xf0000000) == 0) ? 0 /* beq */ : 1 /* bne */;
                  scanp1 = active_inst(scan  ,1);
                  scanp2 = active_inst(scanp1,1);
                  int match0 = ((*scanp1 & 0x0ff000ff) == 0x03a00000) ? 0 : 1;
                  if (match0 == btype) {
                     *scanp4 = (*scanp4 & 0x0fffffff) | (*scanp1 & 0xf0000000);
                  }
                  else {
                     *scanp4 = (*scanp4 & 0x0fffffff) | (*scanp2 & 0xf0000000);
                  }
                  *scanp1 = NOP;
                  *scanp2 = NOP;
                  *scanp3 = NOP;
                  scan = scanp4;
               }
            }
         }
      }
   }
}

// optimizations involving negative immediate constants. Also imm cmp.
static void apply_peepholes4(int *instInfo, int *funcBegin, int *funcEnd)
{
   int *scan, *scanp1, simple, off;

   create_inst_info(instInfo, funcBegin, funcEnd);
   create_bb_info(instInfo, funcBegin, funcEnd);

   for (scan = funcBegin; scan < funcEnd; ++scan) {
      if ((simple = (*scan & 0x0ffff000) == 0x03a00000) || // mov r0, #imm
          (*scan & 0xffffff00) == 0xe3e00000) {            // mvn r0, #X
         if (is_const(scan)) continue;
         scanp1 = active_inst(scan, 1);
         if (instInfo[scanp1-funcBegin] & RI_bb) continue;
         if (simple) {
            if (*scanp1 == 0xe1510000) { // cmp r1, r0
               *scanp1 |= (1<<25) | (*scan & 0xfff);
               *scan = NOP; scan = scanp1;
            }
            else if ((*scanp1 & 0xfff000f0) == 0xe0000090) { // mul rn, rm, rd
               off = arith_off(*scan);
               if (off && (off & (off-1)) == 0) { // power of 2
                  *scanp1 = 0xe1a00000 | ((*scanp1 & RI_Rn) >> 4) |
                            (*scanp1 & RI_Rm) | (popcount32b(off-1) << 7);
                  *scan = NOP; scan = scanp1;
               }
            }
            continue;
         }
         if (*scanp1 == 0xe0410000 && // sub r0, r1, r0
             (*scan & 0xff) != 0xff) { // #X != -256
            *scanp1 = 0xe2810000 | ((*scan & 0xff) + 1); // add r0,r1,#(-X+1)
            *scan = NOP; scan = scanp1;
         }
         else if ((*scan & 0xff) == 0 && *scanp1 == 0xe0000091){ //mul r0,r1,r0
            *scanp1 = 0xe2610000; // rsb r0, r1, #0
            *scan = NOP; scan = scanp1;
         }
      }
   }
}

// Try to eliminate pc-relative constants that are used as load offsets.
// massage pattern generated by compiler for ++/-- operations (r2 sentinel).
static void apply_peepholes4_2(int *instInfo, int *funcBegin, int *funcEnd)
{
   int *scan, *scanp1, *scanp2, off, moff, fp, add;
   int *cnstPtr, *cnstPtrOld = 0;
   int depInited = 0;

   for (scan = funcBegin; scan < funcEnd; ++scan) {
      if ((*scan & 0xffff0000) == 0xe59f0000) { // ldr rd, [pc, #X]
         if (is_const(scan)) continue;
         scanp1 = active_inst(scan, 1);
         if ((*scanp1 & 0xfff00700) == 0xe0800000 || // add rd, rn, rm
             (*scanp1 & 0xfff00700) == 0xe0400000) { // sub rd, rn, rm
            if ((*scan & RI_Rd) != ((*scanp1 & RI_Rm) << 12)) continue;
            // convert "large" pc-relative constants into offsets
            scanp2 = active_inst(scanp1, 1);
            if (((fp = (*scanp2 & 0xff300f00) == 0xed100a00) || // vldr sd, [rn]
                (*scanp2 & 0xff700000) == 0xe5100000) &&       // ldr rd, [rn]
                (*scanp2 & RI_Rn) == ((*scanp1 & RI_Rd) << 4)) {
               moff = (*scanp2 & (fp ? 0xff : 0xfff)) *
                      ((*scanp2 & (1<<23)) ? 1 : -1);
               cnstPtr = (scan + 2 + (*scan & 0xfff)/4);
               off = *cnstPtr;
               if (fp) off /= 4;
               if ((*scanp1 & 0xfff00700) == 0xe0400000) off = -off;
               off += moff;
               add = 0;
               if (off >= 0)
                  add = (1 << 23);
               else
                  off = -off;
               if (off < (fp ? 0x100 : 0x1000)) {
                  *scanp2 = (*scanp2 & (fp ? 0xff70ff00 : 0xff70f000)) |
                            add | (*scanp1 & RI_Rn) | off;
                  *scanp1 = NOP;
                  delete_const(cnstPtr, scan);
                  *scan = NOP;
                  scan = scanp2;
               }
            }
         }
         // precondition code for later ++/-- optimization
         else if (*scanp1 == 0xe1a01000 &&  // mov r1, r0
                  (*scan & RI_Rd) == 0) {
            int *scanp0 = scan;
            scanp2 = active_inst(scanp1, 1);
            if (*scanp2 != 0xe5900000) continue; // ldr r0, [r0]
            int *scanp4 = active_inst(scanp2, 2);
            if (*scanp4 != 0xe5810000) continue; // str r0, [r1]
            if (depInited == 0) {
               create_inst_info(instInfo, funcBegin, funcEnd);
               create_bb_info(instInfo, funcBegin, funcEnd);
               depInited = 1;
            }
            int *rdd = find_def(instInfo,
               &instInfo[scanp4-funcBegin], 2, S_FWD);
            if (rdd == 0) rdd = &instInfo[funcEnd-funcBegin];
            int *rdu = find_use(instInfo,
               &instInfo[scanp4-funcBegin]+1, 2, S_FWD);
            if (rdu && rdu <= rdd) continue;
            int *scanm1 = active_inst(scan,-1);
            int *scanp5 = active_inst(scanp4, 1);
            cnstPtr = (scan + 2 + (*scan & 0xfff)/4);
            if (*scanm1 == 0xe5821000 && // str r1,[r2]
                (cnstPtrOld && *cnstPtr == *cnstPtrOld) &&
                (instInfo[scan-funcBegin] & RI_bb) == 0) {
               delete_const(cnstPtr, scan);
               if (instInfo[scan-funcBegin] & RI_bb)
                  instInfo[scanp1-funcBegin] |= RI_bb;
               *scan = NOP;
               instInfo[scan-funcBegin] = 0;
            } else {
               *scan += 0x2000; // ldr r2, [pc, #X]
               instInfo[scan-funcBegin] += 0x2000;
               cnstPtrOld = cnstPtr;
            }
            if (instInfo[scanp1-funcBegin] & RI_bb)
               instInfo[scanp2-funcBegin] |= RI_bb;
            *scanp1 = NOP;
            instInfo[scanp1-funcBegin] = 0;
            *scanp2 |= 0x20000; // ldr r0, [r2]
            instInfo[scanp2-funcBegin] |= 0x20000;
            *scanp4 += 0x10000; // str r0, [r2]
            instInfo[scanp4-funcBegin] += 0x10000;
            scan = scanp4;
            if (*scanp5 != 0xe1a01000) continue; // mov r1, r0
            rdu = find_def(instInfo, &instInfo[scanp5-funcBegin]+1, 0, S_FWD);
            if (!rdu || active_inst(&funcBegin[rdu-instInfo],-1) != scanp5)
               continue;
            rdd = find_def(instInfo, &instInfo[scanp5-funcBegin]+1, 1, S_FWD);
            if (rdd == 0) rdd = &instInfo[funcEnd-funcBegin];
            rdu = find_use(instInfo, rdd, 1, S_BACK);
            if (funcBegin[rdu-instInfo] != 0xe5810000) continue; // str r0,[r1]
            rdd = find_use(instInfo, &instInfo[scanp5-funcBegin]+1, 1, S_FWD);
            if (rdd != rdu) continue;
            // make sure no funcs or branch targets
            // between scan and rdd
            int *inst, *dep;
            for (inst = &instInfo[scanp4-funcBegin]; inst <= rdd; ++inst)
               if (*inst & RI_bb) break;
            if (inst <= rdd) continue;
            int *scanp3 = active_inst(scanp2, 1);
            if (*scanp0 == NOP && *scanm1 == 0xe5821000 && // str r1,[r2]
                // cnstPtr == cnstPtrOld) {
                (cnstPtrOld && *cnstPtr == *cnstPtrOld)) {
               *scanm1 = NOP;
               instInfo[scanm1-funcBegin] &= RI_bb;
               *scanp2 = NOP;
               instInfo[scanp2-funcBegin] &= RI_bb;
            }
            else {
               *scanp2 |= 0x1000;
               instInfo[scanp2-funcBegin] |= 0x1000;
            }
            *scanp3 |= 0x11000;
            instInfo[scanp3-funcBegin] |= 0x11000;
            *scanp4 |= 0x1000;
            instInfo[scanp4-funcBegin] |= 0x1000;
            *scanp5 = NOP;
            instInfo[scanp5-funcBegin] &= RI_bb;
            // copy up instructions
            for (inst=scanp4+1, dep=&instInfo[scanp4-funcBegin]+1;
                 inst <= &funcBegin[rdd-instInfo]; ++inst, ++dep) {
               if ((*inst & 0xffff0000) == 0xe59f0000) // ldr r0, [pc, #X]
                  rel_pc_ldr(inst-1, inst);
               else
                  *(inst-1) = *inst;
               *(dep-1) = *dep;
            }
            scan = &funcBegin[rdd-instInfo];
            *scan = 0xe5821000; // str r1, [r2]
            *rdd = RI_RdDest | RI_dataW | RI_RnAct | RI_RdAct | 0x21000;
         }
      }
   }
}

// elide away common use of s0 temporary register,
static void apply_peepholes4_5(int *instInfo, int *funcBegin, int *funcEnd)
{
   int *scan, *scanp1;
   int depInited = 0;

   for (scan = funcBegin; scan < funcEnd; ++scan) {
      if ((*scan & 0xfffff0d0) == 0xeeb00040) { // vmov s0, Fm
         if (is_const(scan)) continue;
         scanp1 = active_inst(scan, 1);
         if ((*scanp1 & 0xffbf00ff) == 0xeeb00040) { // vmov Fn, s0
            if (depInited == 0) {
               create_inst_info_f(instInfo, funcBegin, funcEnd);
               create_bb_info(instInfo, funcBegin, funcEnd);
               depInited = 1;
            }
            if ((instInfo[scanp1-funcBegin] & RI_bb) == 0) {
               *scanp1 = *scanp1 | (*scan & 0x2f);
               *scan = NOP;
               scan = scanp1;
            }
         }
      }
   }
}

// handle common integer-multiply-add instruction end-case
static void apply_peepholes4_6(int *instInfo, int *funcBegin, int *funcEnd)
{
   int *scan;

   for (scan = funcBegin; scan < funcEnd; ++scan) {
      if ((*scan & 0xffff0fff) == 0xe52d0004) { // push rd
         if (is_const(scan)) continue;
         int *scanp1 = active_inst(scan, 1);
         if ((*scanp1 & 0xfffff0f0) == 0xe0000090) { // mul r0, rx, ry
            int *scanp2 = active_inst(scanp1, 1);
            if (*scanp2 == 0xe49d1004) { // pop r1
               int *scanp3 = active_inst(scanp2, 1);
               if (*scanp3 == 0xe0810000) { // add r0, r1, r0
                  int Rd = (*scan & RI_Rd);
                  int *scanp4 = active_inst(scanp3, 1);
                  if (*scanp4 == (0xe1a00000 | Rd) && // mov rd, r0
                      (*scanp1 & RI_Rd) != Rd &&
                      (*scanp1 & RI_Rm) != (Rd >> 12)) {
                     *scanp4 = 0xe0200090 | (Rd << 4) | Rd | (*scanp1 & 0xf0f);
                     *scanp3 = *scanp2 = *scanp1 = *scan = NOP;
                     scan = scanp4;
                  }
               }
            }
         }
      }
   }
}

// simplify constant offset added to struct/array indices
static void apply_peepholes4_7(int *instInfo, int *funcBegin, int *funcEnd)
{
   int *scan, *scanm1, *scanp1;
   int *rxu, *rxd, rx, off, off2, inst;
   int depInited = 0;

   for (scan = funcBegin; scan < funcEnd; ++scan) {
      if ((*scan & 0xfff00070) == 0xe0800000 &&  // add rd, rn, rm, lsl #X
          (*scan & 0xf80) >= 0x100) {                // array (of struct)
         if (is_const(scan)) continue;
         scanm1 = active_inst(scan, -1);
         if (((*scanm1 & RI_Rd) >> 12) == (*scan & RI_Rm) &&
             ((*scanm1 & 0xfff00000) == 0xe2800000 ||  // add rI, rS, #X
              (*scanm1 & 0xfff00000) == 0xe2400000)) { // sub rI, rS, #X
            rx = (*scan & RI_Rd) >> 12;
            if ((1 << rx) & ~UREG_MASK) continue;
            if (depInited == 0) {
               create_inst_info(instInfo, funcBegin, funcEnd);
               // create_bb_info(instInfo, funcBegin, funcEnd);
               depInited = 1;
            }
            rxu = find_use(instInfo, &instInfo[scan-funcBegin]+1,
                          rx, S_FWD);
            scanp1 = &funcBegin[rxu-instInfo];
            if ((*scanp1 & 0xff200f00) == 0xed000a00 && // vldr|str
                (rx << 16) == (*scanp1 & RI_Rn)) {
               rxd = find_def(instInfo, rxu+1, rx, S_FWD);
               if (rxd == 0)
                  rxd = find_use(instInfo, &instInfo[funcEnd-funcBegin-1],
                                 rx, S_BACK) + 1;
               off = (arith_off(*scanm1) << (((*scan & 0xf80) >> 7) - 2)) *
                     (((*scanm1 & 0xfff00000) == 0xe2800000) ? 1 : -1);
               while ((rxu = find_use(instInfo, rxu, rx, S_FWD)) &&
                      rxu < rxd) {
                  inst = funcBegin[rxu-instInfo];
                  if ((inst & 0xff200f00) != 0xed000a00) break; // vldr|str
                  off2 = off + (inst & 0xff)*((inst & (1<<23)) ? 1 : -1);
                  if (off2 < -255 || off2 > 255) goto skip_opt;

                  ++rxu;
               }
               if (rxu == 0 || rxu >= rxd) {
                  rxu = &instInfo[scanp1-funcBegin];
                  while ((rxu = find_use(instInfo, rxu, rx, S_FWD)) &&
                         rxu < rxd) {
                     inst = funcBegin[rxu-instInfo];
                     off2 = off + (inst & 0xff)*((inst & (1<<23)) ? 1 : -1);
                     funcBegin[rxu-instInfo] = (inst & 0xff7fff00) |
                       ((off2 >= 0) ? (off2 | (1<<23)) : -off2);

                     ++rxu;
                  }
                  *scan = (*scan & ~RI_Rm) | ((*scanm1 & RI_Rn) >> 16);
                  instInfo[scan-funcBegin] =
                     (instInfo[scan-funcBegin] & ~RI_Rm) |
                     ((*scanm1 & RI_Rn) >> 16);
                  *scanm1 = NOP;
                  instInfo[scanm1-funcBegin] = 0; // &= RI_bb;
               }
            }
         }
      }
skip_opt: ;
   }
}

#if 0
// optimize straightforward FP-temporary memory copy to use faster int.
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
   }
}
#endif

// Get rid of unnecessary mov operations.
static void apply_peepholes6(int *instInfo, int *funcBegin, int *funcEnd,
                             int low, int high, int dofloat)
{
   int *scan, *scanm1, *scanp1, *scanp2;
   int *info, *rxd, *rxu, *rdd, *rdu, *rdt, *rfinal;
   int rx, rd, rxS, rdm1, cond;
   int movMask = (dofloat) ? 0xffbf0fd0 : 0xffff0ff0;
   int movInst = (dofloat) ? 0xeeb00a40 : 0xe1a00000;

   if (dofloat)
      create_inst_info_f(instInfo, funcBegin, funcEnd);
   else
      create_inst_info(instInfo, funcBegin, funcEnd);

   create_bb_info(instInfo, funcBegin, funcEnd);

   // op rx, ...
   // mov rd, rx   --->  op rd, ...
   for (scan = funcBegin; scan < funcEnd; ++scan) {
      if ((*scan & movMask) == movInst && !is_nop(*scan))  {
         if (is_const(scan)) continue;
         if (instInfo[scan-funcBegin] & RI_bb) continue;
         rx = dofloat ?
              ((*scan & RI_Rm)*2 + ((*scan & 0x20) >> 5)) : (*scan & RI_Rm);
         rd = dofloat ?
              (((*scan & RI_Rd) >> 11) + ((*scan & RI_Sd) >> 22)) :
              ((*scan & RI_Rd) >> 12);
         cond = dofloat ? ((rx >= 2 && rx < low) || rx >= high) :
                          (rx >= low && rx < high);
         if (cond) continue;
         if (rd < (dofloat ? 2 : 3)) continue; // extra precaution
         scanm1 = active_inst(scan, -1);
         if ((*scanm1 & movMask) == movInst && // mov rx, ry
             (instInfo[scanm1-funcBegin] & RI_bb)) {
            continue;
         }
         // need dep info to check dest reg for mul...

         if (dofloat) {
            rdm1 = ((*scanm1 & RI_Rd) >> 11) + ((*scanm1 & RI_Sd) >> 22);
            cond = (rdm1 == rx);
         }
         else {
            if ((*scanm1 & 0xfff00f7f) == 0xee100a10 && // vmov
                (*scanm1 & RI_Rd) == ((*scan & RI_Rm) << 12)) {
               *scanm1 = (*scanm1 & ~RI_Rd) | (*scan & RI_Rd);
               *scan = NOP;
               continue;
            }
            rxS = (instInfo[scanm1-funcBegin] & RI_RdDest) ? 12 : 16;
            cond = (((*scanm1 >> rxS) & 0x0f) == rx &&
                    ((*scanm1 >> 25) & 7) <= 2);
         }

         if (cond) {
            rxu = find_use(instInfo, &instInfo[scan-funcBegin]+1, rx, S_FWD);
            rxd = find_def(instInfo, &instInfo[scan-funcBegin]+1, rx, S_FWD);
            if (rxd == 0) rxd = &instInfo[funcEnd-funcBegin];
            if (rxu != 0 && rxu <= rxd) {
               rfinal = find_use(instInfo, rxd, rx, S_BACK);
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
               // if (instInfo[scan-funcBegin] & RI_bb) {
               //   scanp1 = active_inst(scan, 1);
               //   instInfo[scanp1-funcBegin] |= RI_bb;
               // }
               *scan = NOP;
               instInfo[scan-funcBegin] = 0;
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
      // scan = skip_nop(scan, S_FWD);
      if ((*scan & movMask) == movInst && !is_nop(*scan)) {
         if (is_const(scan)) continue;
         rx = dofloat ?
              ((*scan & RI_Rm)*2 + ((*scan & 0x20) >> 5)) : (*scan & RI_Rm);
         rd = dofloat ?
              (((*scan & RI_Rd) >> 11) + ((*scan & RI_Sd) >> 22)) :
              ((*scan & RI_Rd) >> 12);
         cond = dofloat ? ((rd >= 2 && rd < low) || rd >= high) :
                          (rd >= low && rd < high);
         if (cond) continue;
         scanp1 = active_inst(scan, 1);
         scanp2 = active_inst(scanp1, 1);
         info = &instInfo[scan-funcBegin];
         if ((*scanp1 == 0xe3500000 && // switch stmt -- cmp r0, #0; bne x
             (((*scanp2 >> 24) & 0xff) == 0x1a)) ||
             ((*scanp1 & movMask) == movInst && // mov rx, ry
              (*info & RI_bb)) ||
              rd > (dofloat ? 1 : 2)) {         // frame var assignment
             continue;
         }
         info = &instInfo[scanp1-funcBegin];
         rdu = find_use(instInfo, info, rd, S_FWD);
         if (rd == 0 && rdu == 0) continue; // func return value
         rdd = find_def(instInfo, info, rd, S_FWD);
         if (rdd == 0) rdd = &instInfo[funcEnd-funcBegin];
         rfinal = find_use_precede_def(instInfo, rdu, rdd, rd, S_FWD);

         rxd = find_def(instInfo, info, rx, S_FWD);
         if (rxd == 0) rxd = &instInfo[funcEnd-funcBegin];

         if (rfinal <= rxd) {
            for (rdt = info; rdt <= rfinal; ++rdt) if (*rdt & RI_bb) break;
            if (rdt > rfinal) {
               do {
                  int *rscan = &funcBegin[rdu-instInfo];
                  if (dofloat)
                     reg_rename_f(rx, rd, rdu, rscan);
                  else
                     reg_rename(rx, rd, rdu, rscan);
                  if (rdu == rfinal) break;
                  rdu = find_use(instInfo, rdu+1, rd, S_FWD);
               } while (1);
               if (instInfo[scan-funcBegin] & RI_bb)
                  instInfo[scanp1-funcBegin] |= RI_bb;
               *scan = NOP;
               instInfo[scan-funcBegin] = 0;
               scan = scanp1;
            }
         }
      }
   }
}

// optimize autoinc/dec pointer for load/str operations
static void apply_peepholes7(int *instInfo, int *funcBegin, int *funcEnd)
{
   int *scan, *scanm1, *rdt, *info;
   int *rdd, *rdu, *rdd2, *rdu2;
   int rn, inst, inst2, isAdd, isSub, isFP, off;

   create_inst_info(instInfo, funcBegin, funcEnd);
   create_bb_info(instInfo, funcBegin, funcEnd);

   for (scan = funcBegin; scan < funcEnd; ++scan) {
      if (((isFP = (*scan & 0xff200f00) == 0xed000a00) // vldr|str
              && ((*scan & 0xff) < 2))            ||   // zero offset
          (*scan & 0xff200000) == 0xe5000000) {        // (ldr|str)[b]
         if (is_const(scan)) continue;
         rn = (*scan & RI_Rn) >> 16;
         if ((1 << rn) & ~UREG_MASK) continue;
         info = &instInfo[scan-funcBegin];
         rdd = find_def(instInfo, info-1, rn, S_BACK);
         rdu = find_use(instInfo, info-1, rn, S_BACK);
         if (rdu != 0 && rdu > rdd) continue;
         for (rdt = rdd+1; rdt <= info; ++rdt) if (*rdt & RI_bb) break;
         if (rdt <= info) continue;
         inst = funcBegin[rdd-instInfo];
         if (isFP && (inst & 0xff) != 4) continue;
         scanm1 = active_inst(&funcBegin[rdd-instInfo],-1);
         if ((instInfo[scanm1-funcBegin] & RI_memOp) == RI_frameW &&
             (inst & RI_Rd) == (*scanm1 & RI_Rd)) continue;
         if ((isAdd = (inst & 0xfff00000) == 0xe2800000) || // add #X
             (inst & 0xfff00000) == 0xe2400000) {           // sub #X
            off = isFP ? ((*scan & 0xff) * 4) : (*scan & 0xfff);
            if ((inst & RI_Rn) == ((inst & RI_Rd) << 4)) { // pre inc/dec
               if (off == 0) {
                  if (isFP) {
                     if (!isAdd) {
                        *scan = (*scan & 0xff7fff00) | (1 << 21) | 1;
                        funcBegin[rdd-instInfo] = NOP;
                        *rdd &= RI_bb;
                     }
                  }
                  else {
                     *scan = (*scan & 0xff7ff000) |
                             ((isAdd ? 5 : 1) << 21) | arith_off(inst);
                     funcBegin[rdd-instInfo] = NOP;
                     *rdd &= RI_bb;
                  }
               }
               else if (off * ((*scan & (1 << 23)) ? 1 : -1) +
                        arith_off(inst) * (isAdd ? 1 : -1) == 0) { // post
                  if (!isFP || isAdd) {
                     *scan ^= 3 << 23;   // post inc/dec
                     if (isFP) *scan |= (1 << 21);
                     funcBegin[rdd-instInfo] = NOP;
                     *rdd &= RI_bb;
                  }
               }
            }
            else if (off == 0) { // post inc/dec
               inst2 = *scanm1;
               if ((isSub = (inst2 & 0xfff00000) == 0xe2400000) || // sub #X
                   (inst2 & 0xfff00000) == 0xe2800000) {           // add #X
                  if ((inst2 & RI_Rn) == (inst & RI_Rn) &&
                      (inst2 & RI_Rn) == ((inst2 & RI_Rd) << 4) &&
                      (inst & 0xfff) == (inst2 & 0xfff) && isSub == isAdd) {
                     // make sure rn for ldr|str inst
                     // is not used before next def
                     int patch = 0;
                     int *oldscan;
                     rdu2 = find_use(instInfo, info+1, rn, S_FWD);
                     rdd2 = find_def(instInfo, info+1, rn, S_FWD);
                     if (rdd2 == 0) rdd2 = &instInfo[funcEnd-funcBegin];
                     if (rdu2 != 0 && rdu2 <= rdd2) {
                        // check for compound-assign -- last use is a store
                        rdu2 = find_use(instInfo, rdd2, rn, S_BACK);
                        int *str = &funcBegin[rdu2 - instInfo];
                        int inst3 = *str;
                        if (((*scan & 0xff300000) == 0xe5100000 && // ldr
                             (inst3 & 0xff300000) == 0xe5000000) ||
                            ((*scan & 0xff300f00) == 0xed100a00 && // vldr
                             (inst3 & 0xff300f00) == 0xed000a00)) {
                           if (!(isFP && isSub && *(str-1) != NOP)) {
                              patch = 1;
                              *scan = (*scan & ~RI_Rn) | (inst2 & RI_Rn);
                              instInfo[scan-funcBegin] &= ~RI_Rn;
                              instInfo[scan-funcBegin] |= (inst2 & RI_Rn);
                              oldscan = scan;
                              scan = str;
                           }
                           else
                              continue;
                        }
                        else
                           continue;
                     }

                     if (isFP) {
                        if (isSub) {
                           *scan = (*scan & ~RI_Rn) |
                                   (*scanm1 & RI_Rn) | 0x00800001;
                           instInfo[scan-funcBegin] &= ~RI_Rn;
                           instInfo[scan-funcBegin] |= (*scanm1 & RI_Rn);
                           funcBegin[rdd-instInfo] = NOP;
                           *rdd &= RI_bb;

                           if (patch) {
                              *(scan-1) = inst2;
                              *scanm1 = NOP;
                              create_inst_info(instInfo, funcBegin, funcEnd);
                              create_bb_info(instInfo, funcBegin, funcEnd);
                           }
                        }
                        else {
                           *scan = (*scan & 0xfe70ff00) | (inst & RI_Rn) |
                                   ((isAdd ? 0 : 1) << 23) | (1 << 21) | 1;
                           instInfo[scan-funcBegin] &= ~RI_Rn;
                           instInfo[scan-funcBegin] |= (inst & RI_Rn);
                           funcBegin[rdd-instInfo] = *scanm1 = NOP;
                           *rdd &= RI_bb;
                           instInfo[scanm1-funcBegin] &= RI_bb;
                        }
                     }
                     else {
                        *scan = (*scan & 0xfe70f000) | (inst & 0x000f00ff) |
                                ((isAdd ? 0 : 1) << 23);
                        instInfo[scan-funcBegin] &= ~RI_Rn;
                        instInfo[scan-funcBegin] |= (inst & RI_Rn);
                        funcBegin[rdd-instInfo] = *scanm1 = NOP;
                        *rdd &= RI_bb;
                        instInfo[scanm1-funcBegin] &= RI_bb;
                     }

                     if (patch) {
                        scan = oldscan+1;
                     }
                  }
               }
            }
         }
      }
   }
}

// more (cf 4_7) const offset optimization for struct member access indices
static void apply_peepholes7_5(int *instInfo, int *funcBegin, int *funcEnd)
{
   int *scan, *scan2, *info;
   int *scanm, *scanmm, *scanmp, *scanp1, *scanp2, *scanp3;
   int *rxd, *rxu, *rxuu, *rxdd, *rad, *rau, rx, rn, opt, off, off2, inst;
   int *rde, *rue, rename, depInited = 0;
   // int *rdt;

   create_inst_info(instInfo,funcBegin,funcEnd);
   create_bb_info(instInfo,funcBegin,funcEnd);

   // first pass -- consolidate add/mul
   for (scan = funcBegin; scan < funcEnd; ++scan) {
      if ((*scan & 0xffb0ffff) == 0xe5800000 ||  // str[b] r0, [rx]
          (*scan & 0xffb0ffff) == 0xed800a00) {  // vstr   s0, [rx]
         if (is_const(scan)) continue;
         rx = (*scan & RI_Rn) >> 16;
         if ((1 << rx) & ~UREG_MASK) continue;
         info = &instInfo[scan-funcBegin];
         rxd = find_def(instInfo, info-1, rx, S_BACK);
         if (rxd == 0 || (*rxd & RI_func)) continue;
         scanm = &funcBegin[rxd-instInfo];
         if ((*scanm & 0xfff00000) == 0xe2800000) { // add rx, ry, #N
            rxdd = find_def(instInfo, rxd-1, (*scanm & RI_Rn) >> 16, S_BACK);
            if (rxdd == 0 || (*rxdd & RI_func)) continue;
            rxu = find_use(instInfo, rxd-1, (*scanm & RI_Rn) >> 16, S_BACK);
            if (rxu == 0 || rxu <= rxdd) {
               int vshft = (((*scan & 0xffb0ffff) == 0xed800a00) ? 2 : 0);
               rad = find_def(instInfo,rxd+1,(*scanm & RI_Rn) >> 16,S_FWD);
               if (rad < info) {
                  if (rad == 0 || (*rad & RI_func)) continue;
                  rau = find_use(instInfo,rad,(*scanm & RI_Rn) >> 16,S_BACK);
                  if (rau != rxd) continue;
               }
               scanmm = &funcBegin[rxdd-instInfo];
               // check for "sub rN, fp, #X"
               opt = (((*scanmm & 0xffff0000) == 0xe24b0000) ? 2 : 0);
               rxu = find_use(instInfo, rxd+1, rx, S_FWD);
               if (rxu != info) {
                  scanmp = &funcBegin[rxu-instInfo];
                  rxuu = find_use(instInfo, rxu+1, rx, S_FWD);
                  if (rxuu == info &&
                      ((*scanmp & 0xff300fff) == 0xe5100000 &&
                        ((*scanmp >> 16) & 0xf) == rx)) { // ldr[b] xx, [rX]
                     opt = ((opt == 2) ? 1 : 3);
                  }
               }
               else if (opt == 0) opt = 4;

               // for (rdt = rxdd+1; rdt <= info; ++rdt)
               //    if (*rdt & RI_bb) break;
               // if (rdt > info) {
                  switch(opt) { // disable case 1 and 2 to see structs in asm
                  case 1: // best performance
                     // move fp in ldr
                     *scanmp = (*scanmp & 0xff70f000) | (0xb << 16) |
                      ((arith_off(*scanmm) - arith_off(*scanm)) >> vshft);
                  case 2:
                     *scan = (*scan & 0xff70f000) | (0xb << 16) |
                        ((arith_off(*scanmm) - arith_off(*scanm)) >> vshft);
                     *scanmm = NOP;
                     *scanm  = NOP;
                     break;
                  case 3:
                     *scanmp = *scanmp | (arith_off(*scanm) >> vshft);
                  case 4:
                     if (*rxdd & RI_RdDest)
                        *scanmm = (*scanmm & ~RI_Rd) | (rx << 12);
                     else // RI_RnDest
                        *scanmm = (*scanmm & ~RI_Rn) | (rx << 16);
                     *scan = *scan | (arith_off(*scanm) >> vshft);
                     *scanm = NOP;
                  }
               // }
            }
         }
      }
   }

   // second pass -- non power-of-two struct size and multiply-add opt
   for (scan = funcBegin; scan < funcEnd; ++scan) {
      if ((*scan & 0xfff00ff0) == 0xe0000090) { // mul rz, rx, ry
         if (is_const(scan)) continue;
         scanp1 = active_inst(scan,1);
         if ((*scanp1 & 0xfff00ff0) == 0xe0800000) { // add rd, ra, rz
            if ((*scan & RI_Rn) == (*scanp1 & RI_Rn)) {
               *scanp1 = (*scanp1 & 0xfff0fff0) |
                  ((*scanp1 & RI_Rn) >> 16) | ((*scanp1 & RI_Rm) << 16);
            }
            if ((*scan & RI_Rn) != ((*scanp1 & RI_Rm) << 16)) continue;
            *scanp1 = 0xe0200090 | ((*scanp1 & RI_Rd) << 4) |  // mla
                  ((*scanp1 & RI_Rn) >> 4) | ((*scan & RI_Rd) >> 4) |
                  (*scan & RI_Rm);
            *scan = NOP;
            if (depInited) {
               info = &instInfo[scanp1-funcBegin];
               *info = /* (*info & RI_bb) | */ RI_RnDest | RI_Active |
                  (*scanp1 & activeRegMask[RI_Active]) ;
               instInfo[scan-funcBegin] = 0; // &= RI_bb;
            }
            scanp2 = active_inst(scanp1, 1);

            if ((*scanp2 & 0xfff00000) == 0xe2800000 && /* add rx, rI, #N */
                (*scanp1 & RI_Rn) == (*scanp2 & RI_Rn)) {
               rx = (*scanp2 & RI_Rd) >> 12;
               if ((1 << rx) & ~UREG_MASK) continue;
               if (depInited == 0) {
                  create_inst_info(instInfo,funcBegin,funcEnd);
                  depInited = 1;
               }
               rxu = find_use(instInfo, &instInfo[scanp2-funcBegin]+1,
                             rx, S_FWD);
               scanp3 = &funcBegin[rxu-instInfo];
               if ((*scanp3 & 0xff200f00) == 0xed000a00 && // vldr|str
                   (rx << 16) == (*scanp3 & RI_Rn)) {
                  rename = !(((*scanp1 & RI_Rn) < 0x30000) &&
                             ((*scanp2 & RI_Rd) > 0x2000));
                  rn = (*scanp2 & RI_Rn) >> 16; // if renaming
                  info = rxu = &instInfo[scanp3-funcBegin];
                  rxd = find_def(instInfo, info+1, rx, S_FWD);
                  if (rxd == 0)
                     rxd = find_use(instInfo, &instInfo[funcEnd-funcBegin-1],
                                    rx, S_BACK) + 1;
                  off = (arith_off(*scanp2) / 4);
                  while ((rxu = find_use(instInfo, rxu, rx, S_FWD)) &&
                         rxu < rxd) {
                     inst = funcBegin[rxu-instInfo];
                     if ((inst & 0xff200f00) != 0xed000a00) break; // vldr|str
                     off2 = off + (inst & 0xff)*((inst & (1<<23)) ? 1 : -1);
                     if (/* off2 < -255 || */ off2 > 255) goto skip_opt;

                     ++rxu;
                  }
                  if (rxu == 0 || rxu >= rxd) {
                     rde = find_def(instInfo, &instInfo[scanp1-funcBegin]+1,
                                    (*scanp1 & RI_Rn) >> 16, S_FWD);
                     if (rde == 0) rde = &instInfo[funcEnd-funcBegin]-1;
                     rue = find_use(instInfo, &instInfo[scanp2-funcBegin]+1,
                                    (*scanp1 & RI_Rn) >> 16, S_FWD);
                     if (rue && rue <= rde) continue;
                     for (rxu = info; rxu && rxu < rxd;
                          rxu = find_use(instInfo, rxu+1, rx, S_FWD)) {
                        scan2 = &funcBegin[rxu-instInfo];
                        off2 = off + (*scan2 & 0xff) *
                                     ((*scan2 & (1<<23)) ? 1 : -1);
                        *scan2 = (*scan2 & 0xff7fff00) |
                                 ((off2 >= 0) ? (off2 | (1<<23)) : -off2);
                        if (rename)
                           reg_rename(rn, rx, rxu, &funcBegin[rxu-instInfo]);
                     }
                     if (!rename) {
                        // just rename the mla def rather than all uses
                        *scanp1 = (*scanp1 & ~RI_Rn) | (rx << 16);
                        info = &instInfo[scanp1-funcBegin];
                        *info = (*info & ~RI_Rn) | (rx << 16);
                     }
                     *scanp2 = NOP;
                      instInfo[scanp2-funcBegin] = 0; // &= RI_bb;
                  }
               }
            }
            scan = scanp1;
         }
      }
skip_opt: ;
   }
}

// clean up some address calculations
static void apply_peepholes7_6(int *funcBegin, int *funcEnd)
{
   int *scan, *scanp1;

   for (scan = funcBegin; scan < funcEnd; ++scan) {
      if ((*scan & 0xfff00000) == 0xe1a00000 &&
          (*scan & 0xf80) && !(*scan & 0x70)) { // lsl
         if (is_const(scan)) continue;
         scanp1 = active_inst(scan, 1);
         if ((*scanp1 & 0xfff00000) == 0xe1a00000 &&
             (*scanp1 & 0xf80) && !(*scanp1 & 0x70)) { // lsl
            if (((*scan & RI_Rd) >> 12) == (*scanp1 & RI_Rm)) {
               *scanp1 = (*scanp1 & ~RI_Rm) + (*scan & 0xf8f);
               *scan = NOP;
            }
         }
         else if ((*scanp1 & 0xfff00070) == 0xe0800000) {  // add rd, rn, rm
            if (((*scan & RI_Rd) >> 12) == (*scanp1 & RI_Rm)) {
               *scanp1 = (*scanp1 & ~RI_Rm) + (*scan & 0xf8f);
               *scan = NOP; scan = scanp1;
            }
            else if (((*scan & RI_Rd) << 4) == (*scanp1 & RI_Rn)) {
               *scanp1 = (*scanp1 & ~(RI_Rn | RI_Rm)) |
                         ((*scanp1 & RI_Rm) << 16) | (*scan & 0xf8f);
               *scan = NOP; scan = scanp1;
            }
         }
      }
   }
}

// Get rid of another (cf 6) unnecessary mov operation
static void apply_peepholes7_7(int *funcBegin, int *funcEnd)
{
   int *scan, *scanm1, *scanp1;

   for (scan = funcBegin; scan < funcEnd; ++scan) {
      if (*scan == 0xe1a01000) { // mov r1, r0
         if (is_const(scan)) continue;
         scanm1 = active_inst(scan,-1);
         if ((*scanm1 & 0xfff0f070) == 0xe0800000 && // add r0, sn, sm, lsl #X
             (*scanm1 & 0xf80) >= 0x100) {           // array (of struct)
            scanp1 = active_inst(scan, 1);
            if ((*scanp1 & 0xff3f0fff) == 0xed100a00) {// vldr sx, [r0]
               *scanm1 |= 0x1000; *scan = NOP; *scanp1 |= 0x10000;
            }
         }
      }
   }
}

// fold some remaining (cf 4_7, 7_5) constant offsets into array addresses
static void apply_peepholes7_8(int *instInfo, int *funcBegin, int *funcEnd)
{
   int *scan, *scanm, *info, isS;
   int *rxd, *rxu, *rxdd, rx, off, off2, inst;
   int depInited = 0;

   create_inst_info(instInfo,funcBegin,funcEnd);

   // first pass -- consolidate add/mul
   for (scan = funcBegin; scan < funcEnd; ++scan) {
      if ((isS = ((*scan & 0xff200f00) == 0xe5000000)) || // ldr|str[b]
          (*scan & 0xff200f00) == 0xed000a00) {           // vldr|str
         if (is_const(scan)) continue;
         rx = (*scan & RI_Rn) >> 16;
         if ((1 << rx) & ~UREG_MASK) continue;
         info = &instInfo[scan-funcBegin];
         rxd = find_def(instInfo, info-1, rx, S_BACK);
         if (rxd == 0 || (*rxd & RI_func)) continue;
         scanm = &funcBegin[rxd-instInfo];
         if ((*scanm & 0xfff00000) == 0xe2800000 && // add rx, rn, #N
             (*scanm & RI_Rn) != ((*scanm & RI_Rd) << 4)) {
            rxu = find_use(instInfo, rxd+1, rx, S_FWD);
            if (rxu == info) {
               rxdd = find_def(instInfo, info+1, rx, S_FWD);
               if (rxdd == 0)
                  rxdd = &instInfo[funcEnd-funcBegin-1];
               rxdd = find_use(instInfo, rxdd, rx, S_BACK) + 1;
               if (depInited == 0) {
                  create_bb_info(instInfo,funcBegin,funcEnd);
                  depInited = 1;
               }
               for (rxu = rxd+1; rxu < rxdd; ++rxu) if (*rxu & RI_bb) break;
               if (rxu < rxdd) continue;
               rxu = find_def(instInfo, rxd+1, (*scanm & RI_Rn) >> 16, S_FWD);
               if (rxu == 0 || rxu >= rxdd) {
                  off = arith_off(*scanm) / (isS ? 1 : 4);
                  rxu = info;
                  while ((rxu = find_use(instInfo, rxu, rx, S_FWD)) &&
                         rxu < rxdd) {
                     inst = funcBegin[rxu-instInfo];
                     if (isS) {   // ldr|str[b]
                        if ((inst & 0xff200f00) != 0xe5000000) break;
                        off2 = off + (inst & 0xfff)*((inst & (1<<23)) ? 1 : -1);
                        if (/* off2 < -4095 || */ off2 > 4095) goto skip_opt;
                     }
                     else {       // vldr|str
                        if ((inst & 0xff200f00) != 0xed000a00) break;
                        off2 = off + (inst & 0xff)*((inst & (1<<23)) ? 1 : -1);
                        if (/* off2 < -255 || */ off2 > 255) goto skip_opt;
                     }

                     ++rxu;
                  }
                  if (rxu == 0 || rxu >= rxdd) {
                     rxu = info;
                     while ((rxu = find_use(instInfo, rxu, rx, S_FWD)) &&
                            rxu < rxdd) {
                        inst = funcBegin[rxu-instInfo];
                        off2 = off + (inst & (isS ? 0xfff : 0xff))*
                               ((inst & (1<<23)) ? 1 : -1);
                        funcBegin[rxu-instInfo] =
                          (inst & (isS ? 0xff70f000 : 0xff70ff00)) |
                          (*scanm & RI_Rn) |
                          ((off2 >= 0) ? (off2 | (1<<23)) : -off2);
                        *rxu = (*rxu & ~RI_Rn) | (*scanm & RI_Rn);

                        ++rxu;
                     }
                     *scanm = NOP;
                     instInfo[scanm-funcBegin] &= RI_bb;
                  }
               }
            }
         }
      }
skip_opt: ;
   }
}

// get rid of a portion of cruft surrounding func argument preparation.
// get rid of more unnecessary mov/vmov ops.
// get rid of some redundant loads after stores.
// create FP vmla, vmls, vnmul elision instructions.
// create bic instruction from simpler instructions.
static void apply_peepholes8(int *instInfo, int *funcBegin, int *funcEnd_,
                             int flow, int fhigh)
{
   int *scan, *scanm1, *scanp1, *scanp2;
   int *rdt, *info, *rfinal;
   int *rnu, *rnd, *rdu, *rdd, t, iinfo, rn, rd, mask1, mask2;
   int *funcEnd = funcEnd_;
   int idepInited = 0;
   int fdepInited = 0;
   int *finfo;

   for (scan = funcBegin; scan < funcEnd; ++scan) {
      if (*scan == NOP) continue;
      if ((t = *scan == 0xe52d0004) || *scan == 0xed2d0a01) { // [v]push {r0}
         if (is_const(scan)) continue;
         scanp1 = active_inst(scan,1);
         if ((*scanp1 & 0xffff0fff) == 0xe49d0004  || // pop {rn}
             (*scanp1 & 0xffbf0fff) == 0xecbd0a01) {  // vpop {sn}
            if (idepInited == 0) {
               create_inst_info(instInfo, funcBegin, funcEnd);
               create_bb_info(instInfo, funcBegin, funcEnd);
               idepInited = 1;
            }
            if ((instInfo[scanp1-funcBegin] & RI_bb) != 0) continue;
            rd = t ? ((*scanp1 & RI_Rd) >> 12) :
               (((*scanp1 & RI_Rd) >> 11) | ((*scanp1 & RI_Sd) ? 1 : 0));
            if (rd > 9) continue;
            if (rd == 0) {
               *scan   = NOP;
               *scanp1 = NOP;
               scan = scanp1;
               continue;
            }
            else if ((instInfo[scan-funcBegin] & RI_bb) == 0) {
               scanm1 = active_inst(scan,-1);
               if (t) { // int op
                  iinfo = instInfo[scanm1-funcBegin];
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
               else { // FP op
                  if ((*scanm1 & (RI_Rd | RI_Sd)) == 0) {
                     if (fdepInited == 0) {
                        finfo =
                           (int *) malloc((funcEnd-funcBegin+2)*sizeof(int));
                        create_inst_info_f(finfo, funcBegin, funcEnd);
                        fdepInited = 3;
                     }
                     if (finfo[scanm1-funcBegin] & RI_hasD) {
                        *scanm1 |= ((rd << 11) & RI_Rd) |
                                   ((rd & 1) << 22);
                        *scan   = NOP;
                        *scanp1 = NOP;
                        scan = scanp1;
                        continue;
                     }
                  }
               }
            }

            // default action
            *scan   = NOP;
            *scanp1 = (t) ? (0xe1a00000 | (rd << 12)) :          // mov rd, r0
                            (0xeeb00a40 | ((rd << 11) & RI_Rd) | // vmov sd, s0
                                          ((rd & 1) << 22));
            scan = scanp1;
         }
      }
      else if (*scan == 0xe28fe000) { // add lr, pc, #0
         scanm1 = active_inst(scan, -1);
         if (*scanm1 == 0xe49d0004 || *scanm1 == 0xecbd0a01) { // (v)pop r0/s0
            int *scanm3 = active_inst(scanm1, -2);
            if (*scanm3 == 0xe52d0004 || *scanm3 == 0xed2d0a01) { // (v)push r0
               *scanm1 = NOP; *scanm3 = NOP;
            }
         }
      }
      else if (*scan == 0xe3a00000) { // mov r0, #0
         // clean up FP printf prologue
         scanp1 = active_inst(scan, 1);
         if (*scanp1 == 0xe08d0000) { // add r0, sp, r0
            *scanp1 = 0xe1a0000d; // mov r0, sp
            *scan = NOP;
         }
      }
   }

   fdepInited &= 1;
   for (scan = funcBegin; scan < funcEnd; ++scan) {
      if (*scan == NOP || (*scan & 0xffbf0fd0) != 0xeeb00a40) // vmov Fd, Fm
         continue;
      if (is_const(scan)) continue;
      if ((fdepInited & 2) == 0) {
         if ((fdepInited & 1) == 0)
            finfo = (int *) malloc((funcEnd-funcBegin+2)*sizeof(int));
         create_inst_info_f(finfo, funcBegin, funcEnd);
         create_bb_info(finfo, funcBegin, funcEnd);
         fdepInited = 3;
      }
      if ( !(finfo[scan-funcBegin] & RI_bb)) {
         scanm1 = active_inst(scan,-1); // op Fd2, Fn, Fm
         if (((*scanm1 & RI_Rd) >> 12) == (*scan & RI_Rm) &&
             (((*scanm1 & RI_Sd) >> 18) ^ (*scan & 0x10)) == 0 &&
             (*scan & RI_Rm) == 0 && (finfo[scanm1-funcBegin] & RI_hasD) &&
             (*scanm1 & 0xff3f0f00) != 0xed1f0a00) { // vldr s0, [pc, #X]
            // make sure no use of Fd before next def -- extra careful
            info = &finfo[scan-funcBegin] + 1;
            rdu = find_use(finfo, info+1, (*scan & 0x10) >> 4, S_FWD);
            rdd = find_def(finfo, info+1, (*scan & 0x10) >> 4, S_FWD);
            if (rdu == 0 || rdd < rdu) {
               *scan = (*scanm1 & ~(RI_Rd | RI_Sd)) |
                       (*scan & (RI_Rd | RI_Sd));
               *scanm1 = NOP;
               finfo[scan-funcBegin] =
                  (finfo[scan-funcBegin] & (RI_Rd | RI_Sd)) |
                  (finfo[scanm1-funcBegin] & ~(RI_Rd | RI_Sd));
               finfo[scanm1-funcBegin] = 0;
            }
         }
      }
   }

   fdepInited &= 1;
   idepInited = 0;
   --funcEnd;
   for (scan = funcBegin; scan < funcEnd; ++scan) {
      if (*scan == NOP) continue;
      if ((*scan & 0xfffff000) == 0xe50b0000) { // str r0, [fp, #X]
         if (is_const(scan)) continue;
         scanp1 = active_inst(scan,1);
         if ((*scanp1 & 0xfffff000) == 0xe51b0000 && // ldr r0, [fp, #X]
             (*scan & 0xfff) == (*scanp1 & 0xfff)) {
            if (idepInited == 0) {
               create_inst_info(instInfo, funcBegin, funcEnd);
               create_bb_info(instInfo, funcBegin, funcEnd);
               idepInited = 1;
            }
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
            if (idepInited == 0) {
               create_inst_info(instInfo, funcBegin, funcEnd);
               create_bb_info(instInfo, funcBegin, funcEnd);
               idepInited = 1;
            }
            info = &instInfo[scan-funcBegin]+1;
            rfinal = &instInfo[scanp1-funcBegin];
            for (rdt = info; rdt <= rfinal; ++rdt) if (*rdt & RI_bb) break;
            if (rdt > rfinal) {
               scanp2 = active_inst(scanp1, 1);
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
#ifdef COMPACT_BUT_SLOWER
      else if ((*scan & 0xfff00070) == 0xe0800000) { // add rd, rn, rm, lsl #X
         if (is_const(scan)) continue;
         scanp1 = active_inst(scan,1);
         if ((*scanp1 & 0xffb00fff) == 0xe5900000 && // ldr[b]
             (*scanp1 & RI_Rn) == ((*scan & RI_Rd) << 4)) {
            if (idepInited == 0) {
               create_inst_info(instInfo, funcBegin, funcEnd);
               create_bb_info(instInfo, funcBegin, funcEnd);
               idepInited = 1;
            }
            rn = (*scan & RI_Rd) >> 12;
            rnu = find_use(instInfo, &instInfo[scanp1-funcBegin]+1, rn, S_FWD);
            if (rnu == 0) rnu = &instInfo[funcEnd-funcBegin];
            rnd = find_def(instInfo, &instInfo[scanp1-funcBegin]+1, rn, S_FWD);
            if (rnd == 0) rnd = &instInfo[funcEnd-funcBegin];
            if (rnu > rnd) {
               *scanp1 = 0xe7000000 |
                         (*scan & 0x000f0fff) | (*scanp1 & 0x00f0f000);
               instInfo[scanp1-funcBegin] =
                  (instInfo[scanp1-funcBegin] & 0xfff00ff0) |
                  (*scanp1 & (RI_Rn | RI_Rd | RI_Rm));
               *scan = NOP;
               instInfo[scan-funcBegin] &= RI_bb;
            }
         }
      }
#endif
      else if ((*scan & 0xffb00f50) == 0xee200a00) {  // vmul sd, sn, sm
         if (is_const(scan)) continue;
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
               *scanp1 = (*scanp1 & mask1) | (*scan & 0x000f00af); // vmla/vmls
               *scan = NOP;
               if (fdepInited & 2) {
                  finfo[scanp1-funcBegin] =
                     (finfo[scanp1-funcBegin] &
                      ~(RI_Rm | RI_Sm | RI_Rn | RI_Sn)) |
                     (finfo[scan-funcBegin] & (RI_Rm | RI_Sm | RI_Rn | RI_Sn));
                  finfo[scan-funcBegin] &= RI_bb;
               }
            }
            else if (((*scan & RI_Rd) >> 12) == (*scanp1 & RI_Rm) &&
                     ((*scan & RI_Sd) >> 17) == (*scanp1 & 0x20)) {
               scanp2 = active_inst(scanp1, 1); // skip for func return value
               if (*scanp2 == 0xe8bd8800 || *scanp2 == 0xe28bd000) continue;
               // verify scanp1 last use of Rn before next def, otherwise skip
               rn = ((*scanp1 & RI_Rn) >> 15) + ((*scanp1 & 0x80) >> 7);
               if ((rn >= 2 && rn < flow) || rn >= fhigh) continue;
               if ((fdepInited & 2) == 0) {
                  if ((fdepInited & 1) == 0)
                     finfo = (int *) malloc((funcEnd-funcBegin+2)*sizeof(int));
                  create_inst_info_f(finfo, funcBegin, funcEnd);
                  create_bb_info(finfo, funcBegin, funcEnd);
                  fdepInited = 3;
               }
               rdu = &finfo[scanp1-funcBegin];
               rnu = find_use(finfo, rdu+1, rn, S_FWD);
               if (rnu != 0) {
                  rnd = find_def(finfo, rdu+1, rn, S_FWD);
                  if (rnd == 0 || rnu <= rnd) continue;
               }
               // verify that Rm is not used until after next def of Rm
               rd = ((*scan & RI_Rd) >> 11) + ((*scan & RI_Sd) >> 22);
               if ( ((*rdu & RI_Rm) | ((*rdu & RI_Sm) >> 16)) !=
                    (((*rdu & RI_Rd) >> 12) | ((*rdu & RI_Sd) >> 18)) ) {
                  rdd = find_def(finfo, rdu+1, rd, S_FWD);
                  if (rdd == 0) rdd = &finfo[funcEnd-funcBegin];
                  rdd = find_use(finfo, rdd, rd, S_BACK); // last xform
                  if (rdd != rdu) continue;
               }
               rd = ((*scanp1 & RI_Rd) >> 11) + ((*scanp1 & RI_Sd) >> 22);
               rdd = find_def(finfo, rdu+1, rd, S_FWD);
               if (rdd == 0) rdd = &finfo[funcEnd-funcBegin];
               rdd = find_use(finfo, rdd, rd, S_BACK); // last xform
               if (rnu && rnu < rdd) continue;
               for (rdt = rdu+1; rdt <= rdd; ++rdt)
                  if (*rdt & RI_bb) break;
               if (rdt > rdd) {
                  *scanp1 = (*scanp1 & mask2) | ((*scanp1 & RI_Rn) >> 4) |
                            ((*scanp1 & 0x80) << 15) | (*scan & 0x000f00af);
                  *rdu =
                     (*rdu & ~(RI_Rm | RI_Sm | RI_Rn | RI_Sn | RI_Rd | RI_Sd))|
                     ((*rdu & RI_Rn) >> 4) | ((*rdu & RI_Sn) << 1) |
                     (finfo[scan-funcBegin] & (RI_Rn | RI_Sn | RI_Rm | RI_Sm));
                  *scan = NOP;
                  finfo[scan-funcBegin] &= RI_bb;
                  do {
                     rdu = find_use(finfo, rdu+1, rd, S_FWD);
                     if (rdu == 0) break;
                     reg_rename_f(rn, rd,
                                  rdu, &funcBegin[rdu-finfo]);
                  } while (rdu < rdd);
               }
            }
         }
         else if ((*scanp1 & 0xffbf0fd0) == 0xeeb10a40) {  // vneg sd, sm
            if (((*scan & RI_Rd) >> 12) == (*scanp1 & RI_Rm) &&
                ((*scan & RI_Sd) >> 17) == (*scanp1 & 0x20)) {
               if (((*scanp1 & RI_Rd) >> 12) == (*scanp1 & RI_Rm) &&
                   ((*scanp1 & RI_Sd) >> 17) == (*scanp1 & 0x20)) {
                  *scan |= 0x40; //  vnmul
                  *scanp1 = NOP;
                  if (fdepInited & 2) finfo[scanp1-funcBegin] &= RI_bb;
                  scan = scanp1;
               }
               else {
                  if ((fdepInited & 2) == 0) {
                     if ((fdepInited & 1) == 0)
                        finfo =
                           (int *) malloc((funcEnd-funcBegin+2)*sizeof(int));
                     create_inst_info_f(finfo, funcBegin, funcEnd);
                     create_bb_info(finfo, funcBegin, funcEnd);
                     fdepInited = 3;
                  }
                  info = &finfo[scanp1-funcBegin];
                  int rm = ((*info & RI_Rm) | ((*info & RI_Sm)>>16));
                  rdd = find_def(finfo, info + 1, rm, S_FWD);
                  if (rdd == 0) rdd = &finfo[funcEnd-funcBegin];
                  rdu = find_use(finfo, rdd, rm, S_BACK);
                  if (rdu == info) {
                     *scan = (*scan & ~(RI_Rd | RI_Sd)) |
                             (*scanp1 & (RI_Rd | RI_Sd)) | 0x40;
                     finfo[scan-funcBegin] = (*info & (RI_Rd | RI_Sd)) |
                            (finfo[scan-funcBegin] & ~(RI_Rd | RI_Sd));
                     *scanp1 = NOP;
                     *info &= RI_bb;
                     scan = scanp1;
                  }
               }
            }
         }
      }
      else if ((*scan & 0xfff0ffff) == 0xecb00a01) { // vldmia rn!,{s0}
         if (is_const(scan)) continue;
         scanp1 = active_inst(scan,1);
         if ((*scanp1 & 0xffbf0fff) == 0xeeb00a40) { // vmov.f32 sd, s0
            *scan |= (*scanp1 & (RI_Rd | RI_Sd));    // vldmia rn!,{sd}
            *scanp1 = NOP;
            if (fdepInited & 2) finfo[scanp1-funcBegin] &= RI_bb;
            scan = scanp1;
         }
      }
      else if ((*scan & 0xffff0fff) == 0xe3e00000) { // mvn rA, #0
         if (is_const(scan)) continue;
         scanp1 = active_inst(scan,1);
         if ((*scanp1 & 0xfff00ff0) == 0xe0200000) { // eor rB, rx, rA
            scanp2 = active_inst(scanp1,1);
            if ((*scanp2 & 0xfff00ff0) == 0xe0000000) { // and rd, ry, rB
               if (((*scan & RI_Rd) >> 12) == (*scanp1 & 0x0f) &&
                   ((*scanp1 & RI_Rd) >> 12) == (*scanp2 & 0x0f)) {
                  *scanp2 = 0xe1c00000 | (*scanp2 & (RI_Rn | RI_Rd)) |
                            ((*scanp1 & RI_Rn) >> 16); // bic rd, ry. rx
                  *scan = *scanp1 = NOP;
                  if (idepInited) {
                     instInfo[scanp1-funcBegin] &= RI_bb;
                     instInfo[scan  -funcBegin] &= RI_bb;
                  }
                  scan = scanp2;
               }
            }
         }
      }
   }
   ++funcEnd;

   if (fdepInited & 1) free(finfo);
}

// enhance branch prediction (8_1, 8_2) by aggressively moving branch condition
// generation (e.g.  cmp instruction) to execute as early as possible
static void apply_peepholes8_1(int *instInfo, int *funcBegin, int *funcEnd)
{
   int *scan, *scanm1, *scanm2, *scanp1, *scanfb;
   int depInited = 0;

   for (scan = funcBegin; scan < funcEnd; ++scan) {
      if ((*scan & 0xfff0ffff) == 0xe3500000) { // cmp rx, #0
         if (is_const(scan)) continue;
         scanp1 = active_inst(scan, 1);
         int cc = (*scanp1 >> 28) & 0x0f; // condition code
         if (((*scanp1 >> 24) & 0x0f) != 0x0a || cc >= 14) // check cond branch
            continue;

         if (cc == 10 || cc == 11) { // GE -> PL or LT -> MI
            *scanp1 = (*scanp1 & 0x0fffffff) | (((cc == 10) ? 5 : 4) << 28);
         }

         int rn = (*scan & RI_Rn) >> 16;
         scanm1 = active_inst(scan,-1);
         int isMul = ((*scanm1 & 0xffc000f0) == 0xe0000090); // mul/mla
         int isDP  = (((*scanm1 & 0xfc000000) == 0xe0000000) &&
                      ((*scanm1 & 0x01800000) != 0x01000000) && !isMul);
         int rc = (*scanm1 >> (isMul ? 16 : 12)) & 0x0f;
         if (rc == rn && (isMul || isDP)) {
            // check for branch compatability
            int t = (*scanm1 >> 21) & 0x0f; // DP opcode
            if ((isDP && ((t < 2) || t > 11 || t == 8 || t == 9)) || isMul) {
               if (cc == 12 || cc == 13) continue; // GT or LE require work
            }

            if (*scanp1 & (1 << 23)) {  // backward branch
               int xform = 0;
               t = 0xff000000 | (*scanp1 & 0x00ffffff);
               int *bdst = scanp1 + 2 + t;
               scanfb = active_inst(bdst, -1); // should be ptr to cond expr
               if (((*scanfb >> 23) & 0x1f) == 0x14) { // forward branch
                  int sc = 0; // special case
                  scanm2 = active_inst(scanfb, -1);
                  isMul = ((*scanm2 & 0xffc000f0) == 0xe0000090); // mul/mla
                  isDP  = (((*scanm2 & 0xfc000000) == 0xe0000000) &&
                           ((*scanm2 & 0x01800000) != 0x01000000) && !isMul);
                  int rl = ((*scanm2 >> (isMul ? 16 : 12)) & 0x0f); // loopvar?
                  t = (*scanm2 >> 21) & 0x0f; // DP opcode
                  if ((isDP && ((t < 2) || t > 11 || t == 8 || t == 9)) ||
                      isMul) {
                     if (cc == 12 || cc == 13) { // GT or LE
                        if ((*scanm2 & 0xffef0000) == 0xe3a00000) // mov rd, #N
                           sc = 1;
                        else
                           sc = 2; // need explicit compare
                     }
                  }
                  int *fdst = scanfb + 2 + (*scanfb & 0x00ffffff);
                  fdst = skip_nop(fdst, S_FWD);
                  if (fdst <= scanm1) {
                     *scan = NOP;
                     *scanm1 |= (1 << 20);
                  }
                  else if (sc < 2 && (((isMul || isDP) && rl == rc) ||
                           (isDP && rl == ((*scanm1 >> 16) & 0x0f)))) {
                     int allNop = 1;
                     for (int *scan1 = scanfb+1; scan1 < bdst; ++scan1) {
                        if (!is_nop(*scan1)) { allNop = 0; break; }
                     }
                     *scan = NOP;
                     *scanm1 |= (1 << 20);
                     if (sc == 1) { // mov rd, #N for loop init
                        int imm = arith_off(*scanm2);
                        int cond = ((cc == 12) ? (imm > 0) : (imm <= 0));
                        if (!cond)
                           *scanfb = ((scanp1 - scanfb) - 1) | (0xea << 24);
                        else if (allNop)
                           *scanfb = NOP;
                        else
                           *scanfb = ((bdst - scanfb) - 1) | (0xea << 24);
                     }
                     else {
                        *scanm2 |= (1 << 20);
                        if (allNop) {
                           *scanfb = ((scanp1 - scanfb) - 1) |
                                     ((*scanp1 & 0xff000000) ^ (1 << 28));
                        }
                     }
                     xform = 1;
                  }
                  else if (fdst == scan && is_nop(*(scanm2 - 1)) &&
                           (*scanm2 & 0x0f000000) != 0x0a000000) { // branch
                     int allNop = 1;
                     for (int *scan1 = scanfb+1; scan1 < bdst; ++scan1) {
                        if (!is_nop(*scan1)) { allNop = 0; break; }
                     }
                     if (allNop) {
                        *scanfb = ((scanp1 - scanfb) - 1) |
                                  ((*scanp1 & 0xff000000) ^ (1 << 28));
                     }
                     *(scanm2 - 1) = *scanm2;
                     *scanm2 = *scan;
                     *scan = NOP;
                     *scanm1 |= (1 << 20);
                     xform = 1;
                  }
               }
               else { // handle do-while
                  *scan = NOP;
                  *scanm1 |= (1 << 20);
               }
               scan = scanm1;

               if (!xform) continue;
               if (depInited == 0) {
                  create_inst_info(instInfo, funcBegin, funcEnd);
                  create_bb_info(instInfo, funcBegin, funcEnd);
                  depInited = 1;
               }
               if ((instInfo[scan-funcBegin] & RI_bb) != RI_bb) {
                  int *inst = &instInfo[scan-funcBegin];
                  int *rxd = find_def(instInfo, inst-1, rc, S_BACK);
                  if (rxd && rxd < &instInfo[bdst-funcBegin]) {
                     int *rxu = find_use(instInfo, inst-1, rc, S_BACK);
                     if (rxu < &instInfo[bdst-funcBegin]) {
                        // one inst loop counter
                        int *scantmp;
                        scanm1 = scan - 1;
                        inst = &instInfo[scanm1-funcBegin];
                        while (scanm1 > bdst) {
                           if ((*inst & RI_bb) || // jump target
                               ((*scanm1 & 0xff000000) == 0xea000000) || // jmp
                               mflag(*scanm1) ||
                               *scanm1 == 0xe8bd8800) { // pop {fp, pc}
                              break;
                           }
                           --scanm1; --inst;
                        }
                        if ((*inst & RI_bb) && scanm1 > bdst) {
                           scantmp = scanm1-1;
                           while (is_nop(*scantmp) && scantmp > bdst) --scantmp;
                           if (scantmp == bdst) scanm1 = bdst;
                        }
                        scantmp = scanm1;
                        while (!is_nop(*scanm1) && scanm1 < scan) ++scanm1;
                        if (scanm1 - scantmp == 1 &&
                            (instInfo[bdst-funcBegin] & RI_memOp)
                                                        != RI_instR) {
                           *scanm1 = *scantmp;
                           *scantmp = *scan;
                           *scan = NOP;
                        }
                        else if (scanm1 != scan) {
                           *scanm1 = *scan;
                           *scan = NOP;
                        }
                     }
                  }
               }
            }
            else { // fwd branch is always safe
               *scan = NOP;
               *scanm1 |= (1 << 20);
               scan = scanm1;
            }
         }
      }
   }
}

// This algorithm currently moves def/cmp/vmrs as early as possible in code.
// Def and cmp can also be separated by distance, and so cmp can be moved
// up without moving def.
static void apply_peepholes8_2(int *instInfo, int *funcBegin, int *funcEnd,
                               int *fbase)
{
   int *scan, *inst;
   int *finfo;
   int depInited = 0;

   // can be extended to overlap work with loads operations,
   // which are likely to stall. Loads can have a more
   // complex dependency pattern.
   for (scan = funcBegin; scan < funcEnd; ++scan) {
      // this code can be extended to work with vcmpe Fd, Fm
      if ((*scan & 0xffbf0fff) == 0xeeb50ac0) { // vcmpe Fd, #0
         if (is_const(scan)) continue;
         if (depInited == 0) {
            create_inst_info(instInfo, funcBegin, funcEnd);
            create_bb_info(instInfo, funcBegin, funcEnd);
            finfo = (int *) malloc((funcEnd-funcBegin+2)*sizeof(int));
            create_inst_info_f(finfo, funcBegin, funcEnd);
            depInited = 1;
         }
         inst = &finfo[scan-funcBegin];
         int Rd = ((*scan & RI_Rd) >> 11) |
                  ((*scan & RI_Sd) ? 1 : 0);
         int *def = find_def(finfo, inst-1, Rd, S_BACK);
         // Not sure this guard should be here.
         // it is checking that def/cmp are adjacent
         if ((instInfo[def-finfo] & RI_memR) ||
             active_inst(&funcBegin[def-finfo], 1) != scan)
             // find_use(finfo, def+1, Rd, S_FWD) != inst)
            continue;
         if (Rd == 0) { // vcmpe s0, #0
            if (*fbase == 0) continue; // can't rename
            int *def0 = find_def(finfo, inst+1, 0, S_FWD);
            int *use0 = find_use(finfo, inst+1, 0, S_FWD);
            if ((def0 && use0 <= def0) || (use0 && def0 == 0))
               continue;
            int reg;
            *fbase ^= (1 << (reg = avail_reg(*fbase)));
            *scan |= ((reg & 0x1e) << 11) | ((reg & 1) ? RI_Sd : 0);
            funcBegin[def-finfo] |= ((reg & 0x1e) << 11) |
                                    ((reg & 1) ? RI_Sd : 0);
         }
         // search back for earliest insertion point
         int *stop = funcBegin;
         int info = *def;
         if (info & RI_RmAct) {
            int *d = find_def(finfo, def-1, ((info & RI_Rm) |
                              ((info & RI_Sm)>>16)), S_BACK);
            int *st = &funcBegin[d-finfo];
            if (st > stop) stop = st;
         }
         if (info & RI_RnAct) {
            int *d = find_def(finfo, def-1, (((info & RI_Rn)>>16) |
                              ((info & RI_Sn)>>17)), S_BACK);
            int *st = &funcBegin[d-finfo];
            if (st > stop) stop = st;
         }
         int *scanm1 = &funcBegin[def-finfo];
         int *start = scanm1;
         inst = &instInfo[scanm1-funcBegin];
         while (scanm1 > stop) {
            if ((*inst & RI_bb) || // jump target
                ((*scanm1 & 0xff000000) == 0xea000000) || // jmp
                mflag(*scanm1) ||
                *scanm1 == 0xe8bd8800) { // pop {fp, pc}
               break;
            }
            --scanm1; --inst;
         }
         if (scanm1 == start) continue;
         while (!is_nop(*scanm1) && scanm1 < scan) ++scanm1;
         if (scanm1 < scan) {
            int *scanN = scanm1+1;
            while (!is_nop(*scanN) && scanN < scan) ++scanN;
            if (scanN < scan) {
               int *scanNN = scanN+1;
               while (!is_nop(*scanNN) && scanNN < scan) ++scanNN;
               if (scanNN < scan) {
                  *scanm1 = funcBegin[def-finfo];
                  *scanN  = *scan;
                  *scanNN = 0xeef1fa10; // vmrs  APSR_nzcv, fpscr
                  funcBegin[def-finfo] = NOP;
                  *scan = NOP;
                  *active_inst(scan, 1) = NOP;
               }
            }
         }
      }
   }

   if (depInited) free(finfo);
}

// use a shift and add in place of a multiply for two-bit literal constants
// immediately before a return statement, remove unnecessary mov r0/s0
// optimize "nearby" div/mod use
static void apply_peepholes9(int *instInfo, int *funcBegin, int *funcEnd,
                             int ireg)
{
   int isFP, *scan, *scanm1, *scanm2;
   int depInited = 0;
   int num_recip = 0;
   int *rinst[32];
   int *linst[32];
   int *raddr[32];
   int *rdef[32];

   for (scan = funcBegin; scan < funcEnd; ++scan) {
      if (*scan == NOP) continue;
      if ((*scan & 0xfff000f0) == 0xe0000090) { // mul rn, rm, rd
         if (is_const(scan)) continue;
         scanm1 = active_inst(scan, -1);
         if ((*scanm1 & 0xffef0000) == 0xe3a00000) { // mov rd, #N
            int addend, high, low, off = arith_off(*scanm1);
            // MUL has less latency than two dependent eqns
            // so "temporarily" disable case where "(off & 1) == 0"
            if (off < 0 || (off & 1) == 0) continue;
            int nbits = popcount32b(off);
            int ss = 32 - nbits - clz(off); // subend shift
            int tval = off + ((1 << ss) - 1);
            if ( ! (nbits == 2 || (nbits > 2 && ((tval+1) & tval) == 0)))
               continue;
            int op = (nbits == 2) ? 0xe0800000 : 0xe0600000; // add : rsb
            if (off & 1) {
               if (nbits > 2) off += 2;
               addend = (((*scanm1 & RI_Rd) >> 12) == (*scan & RI_Rm)) ?
                     ((*scan & RI_Rs) >> 8) : (*scan & RI_Rm);
               high = 31 - clz(--off);
               *scanm1 = NOP;
               if (depInited) instInfo[scanm1 - funcBegin] = 0;
            }
            else {
               if (nbits == 2) {
                  high = 31 - clz(off);
                  low = 31 - clz(off - (1 << high));
                  high -= low;
               }
               else { // subtraction
                  high = 31 - clz(off) - ss + 1;
                  low = ss;
               }
               addend = (*scanm1 & RI_Rd) >> 12; // or subend
               *scanm1 = 0xe1a00000 | (*scanm1 & RI_Rd) | (low << 7) |
                         ((addend == (*scan & RI_Rm)) ?
                         ((*scan & RI_Rs) >> 8) : (*scan & RI_Rm));
            }
            *scan = op | ((*scan & RI_Rn) >> 4) | (high << 7) |
                    (addend << 16) | addend;
         }
      }
      else if (*scan == 0xe28bd000) { // add sp, fp, #0
         if (is_const(scan)) continue;
         scanm1 = active_inst(scan, -1);
         if ((*scanm1 & 0xfff0ffff) == 0xe1a00000 ||  // mov r0, rX
             (*scanm1 & 0xfffff0d0) == 0xeeb00040) { // vmov s0, sX
            if (depInited == 0) {
               create_inst_info(instInfo, funcBegin, funcEnd);
               create_bb_info(instInfo, funcBegin, funcEnd);
               depInited = 1;
            }
            if ((instInfo[scanm1 - funcBegin] & RI_bb) != 0) continue;
            scanm2 = active_inst(scanm1, -1);
            if ((isFP = ((*scanm2 & (15 << 24)) == (14 << 24))) || // DP op
                (*scanm2 & (7 << 25)) <= (1 << 25)) {
               *scanm2 &= isFP ? 0xffbf0fff :
                  ((instInfo[scanm2 - funcBegin] & RI_RdDest) ?
                   0xffff0fff : 0xfff0ffff);
               *scanm1 = NOP;
               instInfo[scanm1 - funcBegin] = 0;
            }
         }
      }
      else if ((*scan & 0xfff0f0f0) == 0xe750f010) {  // smmul
         if (is_const(scan)) continue;
         scanm1 = active_inst(scan, -1);
         if ((*scanm1 & 0xffff0000) == 0xe59f0000) { // ldr rx, [pc, #X]
            if (num_recip == 32) continue;
            raddr[num_recip] = scan + 2 + (*scanm1 & 0xfff) / 4;
            linst[num_recip] = scanm1;
            rinst[num_recip++] = scan;
         }
      }
   }

   // This is a large vanity optimization, it might make sense to jettison
   if (num_recip > 1) {
      int i, j, k, dst, dreg, oset, *def, *fuse, *fdef;
      i = 0;
      if (depInited == 0) {
         create_inst_info(instInfo, funcBegin, funcEnd);
         depInited = 1;
      }

      for (k=0; k < num_recip; ++k) {
         def = find_def(instInfo, &instInfo[rinst[k]-funcBegin],
                        (*rinst[k] & RI_Rm), S_BACK);
         rdef[k] = &funcBegin[def - instInfo];
      }

      while (i + 1 < num_recip) {
         if (raddr[i] == raddr[i+1]) {
            j = i + 2;
            while (j < num_recip && raddr[j-1] == raddr[j]) ++j;
            --j;  // on top of last value

            dreg = *rinst[i] & RI_Rm;
            def = find_def(instInfo, &instInfo[rinst[i]-funcBegin]+1,
                           dreg, S_FWD);
            if (def) {
               k = j;
               while (k > i) {
                  if (*rdef[k] != *rdef[i])
                     --k;
                  else
                     break; // could be premature, due to Rm match?
               }
               if (k == i) {
                  i = j + 1;
                  continue;
               }
               else j = k;
            }
            int mode = 0;
            int *scanp = active_inst(rinst[i], 1);
            if ((*scanp & 0xfff00000) == 0xe0800000 &&
                ((*scanp & RI_Rn) >> 16) == dreg) { // add
               mode = 1;
               for (k = i+1; k <= j; ++k) {
                  scanp = active_inst(rinst[k], 1);
                  if ((*scanp & 0xfff00000) != 0xe0800000 ||
                      ((*scanp & RI_Rn) >> 16) != (*rinst[k] & RI_Rm)) {
                     if (i == k - 1)
                        mode = 0;
                     else
                        j = k - 1;
                     break;
                  }
               }
            }

            scanp = active_inst(rinst[i], 1 + mode);
            if ((*scanp & 0xfff00070) == 0xe1a00040) { // asr
               int shift = *scanp & 0xf80;
               for (k = i+1; k <= j; ++k) {
                  scanp = active_inst(rinst[k], 1 + mode);
                  if ((*scanp & 0xfff00ff0) != (0xe1a00040 | shift)) { // asr
                     if (i == k - 1 || mode == 1)
                        goto next_check;
                     j = k - 1;
                     break;
                  }
               }
               mode |= 2;
            }

next_check:
            for (scanp = rinst[i]; scanp < rinst[j]; ++scanp) {
               if (*scanp == 0xe28fe000 ||  // check for function call
                   (*scanp & 0xff000000) == 0xeb000000)
                  break;
            }
            if (scanp != rinst[j]) {
               i = j + 1;
               continue;
            }

            dst = 2;
            fuse = find_use(instInfo, &instInfo[rinst[i]-funcBegin]+1,
                            dst, S_FWD);
            if (fuse) {
               if (&funcBegin[fuse - instInfo] < rinst[j])
                  dst = avail_reg(ireg);
               else {
                  fdef = find_def(instInfo, &instInfo[rinst[i]-funcBegin]+1,
                                  dst, S_FWD);
                  if (fdef && (fuse <= fdef ||
                      &funcBegin[fdef - instInfo] < rinst[j]))
                     dst = avail_reg(ireg);
               }
            }
            if (dst == -1) {
               i = j + 1;
               continue;
            }

            oset = 0;
            if (mode == 0)
               scanp = rinst[i];
            else {
               oset = popcount32b(mode);
               scanp = active_inst(rinst[i], oset);
            }
            int *info = &instInfo[scanp - funcBegin];
            dreg = (*info & RI_RdDest) ?
                   ((*info & RI_Rd) >> 12) : ((*info & RI_Rn) >> 16);
            fdef = find_def(instInfo, info+1, dreg, S_FWD);
            if (fdef == 0) fdef = &instInfo[rinst[i+1] - funcBegin];
            fuse = find_use(instInfo, info+1, dreg, S_FWD);
            do {
               reg_rename(dst, dreg, fuse, &funcBegin[fuse-instInfo]);
               fuse = find_use(instInfo, fuse+1, dreg, S_FWD);
            } while (fuse && fuse <= fdef);
            if (*info & RI_RdDest)
               *scanp = (*scanp & ~RI_Rd) | (dst << 12);
            else
               *scanp = (*scanp & ~RI_Rn) | (dst << 16);

            for (k = i+1; k <= j; ++k) {
               delete_const(raddr[k], linst[k]);
               *linst[k] = NOP;
               if (rdef[i] != rdef[k]) *rdef[k] = NOP;
               if (mode == 0)
                  *rinst[k] = 0xe1a00000 | ((*rinst[k] & RI_Rn) >> 4) | dst;
               else {
                  *rinst[k] = NOP;
                  scanp = active_inst(rinst[k], 1);
                  if (oset == 2) {
                     *scanp = NOP;
                     scanp = active_inst(scanp, 1);
                  }
                  *scanp = 0xe1a00000 | (*scanp & RI_Rd) | dst;
               }
            }
            i = j + 1;
         }
         else ++i;
      }
   }
}

static int apply_ptr_cleanup(int *instInfo, int *funcBegin, int *funcEnd,
                             int base)
{
   int *scan, *scan2, *scanp1, *scanp1p, inst, i, j, rd;
   int *scanm, *scan2m, mask;
   int off, num_off, jreg;
   int *rdt, *info, *rfinal, *rm, *rn, *ru;
   int *rda, *rdb;
   int np = 0;
   int *finfo;
   int ren_reg, first_op, first_ptr;
   int freg = base, depInited = 0;
   int foff[32];
   int *last_vstr_ptr[32], *last_vstr[32];
   int vstr_ptr_inst[32], vstr_inst[32];
   int reg[32];
   struct ptr_s {
      int inst, mlaval;
      int *first, *last;
   } ptr[60];

   // need to tighten this so it can only apply to
   // base pointer operations.  Right now, add and mla
   // instructions could be accidentally wiped out.
   for (scan = funcBegin; scan < funcEnd; ++scan) {
      if (((*scan & 0xfff00070) == 0xe0800000 && // add rd, rn, rm, lsl #2+
           (*scan & 0xf80) >= 0x100) ||          // array (of struct)
          (*scan & 0xfff000f0) == 0xe0200090) {  // mla
         if (is_const(scan)) continue;
         if (depInited == 0) {
            create_inst_info(instInfo, funcBegin, funcEnd);
            create_bb_info(instInfo, funcBegin, funcEnd);
            depInited = 1;
         }
         info = &instInfo[scan-funcBegin];
         if (*info & RI_RnDest) {
            rd = ((*info & RI_Rn) >> 16);
            scanm = active_inst(scan, -1);
         }
         else
            rd = ((*info & RI_Rd) >> 12);

         scanp1 = active_inst(scan, 1);
         if ((*scanp1 & 0xffff0fff) == 0xe52d0004 && // push {rd}
             (*scanp1 & RI_Rd) == (rd << 12)) {
            continue;
         }
clear_next_dup:
         rfinal = find_def(instInfo, info + 1, rd, S_FWD);
         if (rfinal) {
            scan2 = &funcBegin[rfinal-instInfo];
            if ((*scan2 & 0xfffff000) == 0xe3a00000) { // mov r0, #X
               scanp1 = active_inst(scan2, 1);
               if ((*scanp1 & 0xfff00ff0) == 0xe0200090) // mla
                  rfinal = &instInfo[scanp1-funcBegin];
            }
            scan2 = &funcBegin[rfinal-instInfo];
            // clean out obvious duplicate pointer calculations
            if (*scan == *scan2) {
               if (*info & RI_RnDest) {
                  scan2m = active_inst(scan2, -1);
                  // still need to verify prev inst is load immediate
                  if (*scanm != *scan2m) continue;
               }
               for (rdt = info+1; rdt <= rfinal; ++rdt)
                  if (*rdt & RI_bb) break;
               if (rdt <= rfinal) continue;

               // make sure rvalue registers are not redefined
               // between scan and scan2
               if (( 1 << ((*scan & RI_Rn) >> 16)) & ~UREG_MASK) continue;
               if ((*info & RI_RdDest) && (*info & RI_RnAct)) {
                  if (((*info & RI_Rn) >> 16) != rd) {
                     rdt = find_def(instInfo, info+1,
                                 ((*info & RI_Rn) >> 16), S_FWD);
                     if (rdt && rdt < rfinal) continue;
                  }
               }
               if ((*info & RI_RnDest) && (*info & RI_RdAct)) {
                  if (((*info & RI_Rd) >> 12) != rd) {
                     rdt = find_def(instInfo, info+1,
                                 ((*info & RI_Rd) >> 12), S_FWD);
                     if (rdt && rdt < rfinal) continue;
                  }
               }
               if (*info & RI_RsAct) { // must be mla
                  if (((*info & RI_Rs) >> 8) != rd &&
                      (*scan & RI_Rs) != ((*scanm & RI_Rd) >> 4)) {
                     rdt = find_def(instInfo, info+1,
                                 (*info & RI_Rs) >> 8, S_FWD);
                     if (rdt && rdt < rfinal) continue;
                  }
               }
               if (*info & RI_RmAct) {
                  if ((*info & RI_Rm) != rd) {
                     rdt = find_def(instInfo, info+1,
                                 (*info & RI_Rm), S_FWD);
                     if (rdt && rdt < rfinal) continue;
                  }
               }
               for (rdt = info + 1; rdt <= rfinal; ++rdt)
                  if (*rdt & RI_bb) break;

               if (rdt > rfinal) {
                  scanp1 = active_inst(scan, 1);
                  scanp1p = active_inst(scan2, 1);
                  if (*info & RI_RnDest) {
                     *scan2m = NOP;
                     instInfo[scan2m-funcBegin] &= RI_bb;
                  }
                  if (*scanp1 == *scanp1p &&
                      (*scanp1 & 0xff300f00) == 0xed100a00) {
                     *scanp1p = NOP;
                     instInfo[scanp1p-funcBegin] &= RI_bb;
                  }
                  *scan2 = NOP;
                  instInfo[scan2-funcBegin] &= RI_bb;
                  goto clear_next_dup;
               }
            }
         }

         // prepare for next loop
         inst = *scan & ((*info & RI_RdDest) ?
                0xffff0fff : 0xfff0ffff);
         for (i=0; i<np; ++i) if (ptr[i].inst == inst) break;
         if (i == np) {
            ptr[i].inst = inst;
            ptr[i].mlaval = (*info & RI_RnDest) ? *scanm : 0;
            ptr[i].first = ptr[i].last = scan;
            ++np;
         }
         else
            ptr[i].last = scan;
      }
   }

   // screen out non-obvious candidates
   for (i=0; i<np; ++i) {
      if (ptr[i].first == ptr[i].last) goto quick_delete;
      scan = ptr[i].first;
      info =  &instInfo[ptr[i].first - funcBegin];
      rfinal = &instInfo[ptr[i].last - funcBegin];
      rn = find_def(instInfo, info+1, (*info & RI_RdDest) ?
         ((ptr[i].inst & RI_Rn) >> 16) : ((ptr[i].inst & RI_Rd) >> 12), S_FWD);
      rm = find_def(instInfo, info+1, ptr[i].inst & RI_Rm, S_FWD);
      // rs is always a multiplier set on the line before, so no check

      if ((rm != 0 && rm < rfinal) ||
          (rn != 0 && rn < rfinal)) {
quick_delete:
         if (i < --np)
            memcpy(&ptr[i], &ptr[i+1], (np-i)*sizeof(struct ptr_s));
         --i; // ++p will reset this
      }
   }

   // We need the FP and Int dependency maps here
   if (np != 0) {
      create_inst_info(instInfo, funcBegin, funcEnd);
      create_bb_info(instInfo, funcBegin, funcEnd);
      finfo = (int *) malloc((funcEnd-funcBegin+2)*sizeof(int));
      create_inst_info_f(finfo, funcBegin, funcEnd);
      create_bb_info(finfo, funcBegin, funcEnd);
   }

   for (i=0; i<np; ++i) { // use local vars
      int maxReg = popcount32b(freg);
      if (maxReg == 0) break;
      first_op = 0; first_ptr = 0; num_off = 0;
      for (j=0; j<32; ++j) vstr_ptr_inst[j] = 0;
      scan = ptr[i].first;
      do {
         info = &instInfo[scan-funcBegin];
         rd = (*info & RI_RdDest) ?
              ((*scan & RI_Rd) >> 12) : ((*scan & RI_Rn) >> 16);
         rfinal = find_def(instInfo, info+1, rd, S_FWD);
         if (rfinal == 0) rfinal = &instInfo[funcEnd-funcBegin];
         ru = find_use(instInfo, info+1, rd, S_FWD);
         if (ru == 0 || ru > rfinal) { printf("bad code gen\n"); exit(-1); }
         do {
            scan2 = &funcBegin[ru-instInfo];
            if ((*scan2 & 0xff300f00) == 0xed000a00) { // vstr
               off = *scan2 & 0x008000ff;
               for (j=0; j<num_off; ++j) if (foff[j] == off) break;
               if (j == num_off) {
                  if (num_off == maxReg) {
                     printf("internal error. out of FP regs.\n"); exit(-1);
                  }
                  freg ^= (1 << (reg[num_off] = avail_reg(freg)));
                  foff[num_off++] = off;
               }
               jreg = reg[j];
               info = &finfo[ru-instInfo];
               ren_reg = ((*info & RI_Rd) >> 12) | ((*info & RI_Sd) >> 18);

               // find def of ren_reg above
               rda = find_def(finfo, info-1, ren_reg, S_BACK);
               // find def of ren_reg below
               rdb = find_def(finfo, info+1, ren_reg, S_FWD);
               if (rdb == 0) rdb = &finfo[rfinal-instInfo];
               // find use of ren_reg above last def
               rdb = find_use(finfo, rdb, ren_reg, S_BACK);

               // rename def
               scanp1 = &funcBegin[rda-finfo];
               *scanp1 = (*scanp1 & ~(RI_Rd | RI_Sd)) |
                         ((jreg & 0x1e) << 11) | ((jreg & 1) ? RI_Sd : 0);
               *rda = (*rda & ~(RI_Rd | RI_Sd)) |
                      ((jreg & 0x0f) << 12) | ((jreg & 0x10) ? RI_Sd : 0);

               // rename all uses
               do {
                  rda = find_use(finfo, rda+1, ren_reg, S_FWD);
                  reg_rename_f(jreg, ren_reg,
                               rda, &funcBegin[rda-finfo]);
                  if (info == rda) {
                     last_vstr_ptr[j] = scan;
                     last_vstr[j] = scan2;
                     vstr_ptr_inst[j] = *scan;
                     vstr_inst[j] = *scan2;
                     *scan2 = NOP;
                     *ru &= RI_bb; *rda &= RI_bb;
                  }
               } while (rda < rdb);
            }
            else if ((*scan2 & 0xff300f00) == 0xed100a00) { // vldr
               off = *scan2 & 0x008000ff;
               for (j=0; j<num_off; ++j) if (foff[j] == off) break;
               if (j == num_off) {
                  first_op |= 1 << j;
                  if (num_off == maxReg) {
                     printf("internal error. out of FP regs.\n"); exit(-1);
                  }
                  freg ^= (1 << (reg[num_off] = avail_reg(freg)));
                  foff[num_off++] = off;
               }
               jreg = reg[j];
               rda = &finfo[ru-instInfo];
               ren_reg = ((*rda & RI_Rd) >> 12) | ((*rda & RI_Sd) >> 18);
               rdb = find_def(finfo, rda+1, ren_reg, S_FWD);
               if (rdb == 0) rdb = &finfo[rfinal-instInfo];
               // find use of ren_reg above last def
               rdb = find_use(finfo, rdb, ren_reg, S_BACK);
               if (first_op & (1 << j)) {
                  *scan2 = (*scan2 & ~(RI_Rd | RI_Sd)) |
                            ((jreg & 0x1e) << 11) | ((jreg & 1) ? RI_Sd : 0);
                  *rda = (*rda & ~(RI_Rd | RI_Sd)) |
                         ((jreg & 0x0f) << 12) | ((jreg & 0x10) ? RI_Sd : 0);
                  first_ptr = 1;
               }
               else {
                  *scan2 = NOP;
                  *ru &= RI_bb; *rda &= RI_bb;
               }
               do {
                  rda = find_use(finfo, rda+1, ren_reg, S_FWD);
                  reg_rename_f(jreg, ren_reg,
                               rda, &funcBegin[rda-finfo]);
               } while (rda < rdb);
               first_op &= ~(1 << j);
            }
            ru = find_use(instInfo, ru+1, rd, S_FWD);
         } while (ru != 0 && ru < rfinal);
         mask = ((instInfo[scan-funcBegin] & RI_RdDest) ?
                0xffff0fff : 0xfff0ffff);
         if (!first_ptr) {
            if (ptr[i].mlaval != 0) {
               scanm = active_inst(scan, -1);
               *scanm = NOP;
               instInfo[scanm-funcBegin] &= RI_bb;
            }
            *scan = NOP;
            instInfo[scan-funcBegin] &= RI_bb;
            finfo[scan-funcBegin] &= RI_bb;
         }
         else first_ptr = 0;
         if (scan != ptr[i].last) {
            while((*(++scan) & mask) != ptr[i].inst);
         }
         else break;
      } while (scan <= ptr[i].last);
      for (j=0; j<num_off; ++j) {
         if (vstr_ptr_inst[j] != 0) {
            if (ptr[i].mlaval != 0) {
               scanm = active_inst(last_vstr_ptr[j], -1);
               if (*scanm != ptr[i].mlaval)
                  *(last_vstr_ptr[j] - 1) = ptr[i].mlaval;
            }
            *last_vstr_ptr[j] = vstr_ptr_inst[j];
            *last_vstr[j] = vstr_inst[j];
         }
      }
   }

   if (np != 0) free(finfo);

   return freg;
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
            if (*scan != 0xe8bd8800 || // keep dead sentinel "pop {fp, pc}"
                (*(scan-2) == 0xe8bd8800 && !is_const(scan-2))) {
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
      tmp += (retVal - dstInst);
      *branchInst = 0xea000000 | (tmp & 0x00ffffff);
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
      if ((*scan & 0xff000000) == 0xea000000) { // uncond branch
         if (!is_const(scan)) rethread_branch(scan);
      }
   }
}

/* mc has condtional branches that jump to a compare instruction */
/* follow chains of these instructions to go directly to final address */
static void simplify_branch3(int *funcBegin, int *funcEnd) {
   int *scan;
   int match, tmp;
   for (scan = funcBegin; scan < funcEnd; ++scan) {
      if (*scan == 0xe3500000) { /* cmp r0, #0 */
         if (is_const(scan)) continue;
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
      if (*scan == 0xe3500000) { /* cmp r0, #0 */
         int *scanm1, *scanm2, *scanm3, *scanp1;

         if (is_const(scan)) continue;
         /* simplify cmp blocks */
         scanm1 = active_inst(scan,   -1);
         scanm2 = active_inst(scanm1, -1);
         scanm3 = active_inst(scanm2, -1);
         if ((*scanm3 & 0xf3f0f000) == 0xe1500000)  { /* cmp rX, r0 */
            scanp1 = active_inst(scan, 1);
            int tmp = (*scanp1 & 0x00ffffff) |
                      ((*scanp1 & 0x00800000) ? 0xff000000 : 0);
            int *dst = scanp1 + 2 + tmp;
            if (*dst == 0xe52d0004 || *dst == 0xe49d1004) // push r0 || pop  r1
               continue; // create a boolean value rather than just a branch

            int btype = ((*scanp1 & 0xf0000000) == 0) ? 0 /* eq */ : 1 /* ne */;
            int match0 = ((*scanm1 & 0x0ff000ff) == 0x03a00000) ? 0 : 1;
            if (match0 == btype) {
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
      if ((*scan & 0x0fffffff) == 0x0a000000 &&  // cond jump past next inst
          ((*scan >> 28) &0xf) < 0xe &&
          (scan[1] & 0xff000000) == 0xea000000) {
         if (is_const(scan) || is_const(scan+1)) continue;
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
/* funcEnd points at last "active" instruction , which could be a NOP */
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
   int offset;
   int *scan, *packed;
   int filter = ((mode == InterFunc) ? 0xff : 0x0f) << 24;
   int modeMask = ((mode == InterFunc) ? 0xeb : 0x0a) << 24;
   int *funcStart;

   if (funcBegin[10] != 0xebfffffe)
      funcStart = funcBegin;
   else
      funcStart = (mode == InterFunc) ? (funcBegin + 11) : funcBegin;

   struct pd_s *cremap;
   struct ia_s **inst;
   int lowc, highc;
   int cremap_size = 0;

   if (mode == IntraFunc) {
      // Relocate instruction stream consts
      lowc  = find_const((funcBegin-cbegin)*4);
      highc = find_const((funcEnd+1-cbegin)*4); // past end of func

      for (ii=lowc; ii<highc; ++ii) {
         if (cnst_pool[ii].inst == 0) {
            pack_const(ii);
            break;
         }
      }
      highc = find_const((funcEnd+1-cbegin)*4); // value can change after pack

      if (highc != lowc) {
         if (highc == cnst_pool_size ||
             cnst_pool[highc].data_addr > (funcEnd-cbegin)*4) {
            --highc;
         }
         cremap_size = highc - lowc + 1;
         cremap = (struct pd_s *) calloc(cremap_size, sizeof(struct pd_s));
      }
   }

   /* count number of branches to relocate */
   int nopCount = 0;
   int branchCount = 0;

   for (scan = funcStart; scan < funcEnd; ++scan) {
      if (mode == IntraFunc)
         while (scan < funcEnd && is_const(scan)) ++scan;

      // legal since rename_nop occurs before this call...
      if (*scan == NOP) {
         ++nopCount;
      }
      else if ((*scan & filter) == modeMask) {
         ++branchCount;
      }
   }

   if (branchCount != 0) {
      memblk = (int *) malloc(3*branchCount*sizeof(int)); // 3 arrays
      branchAddr = memblk;
      branchTarget = branchAddr + branchCount;
      permutation = branchTarget + branchCount;

      /* record all branch inst and target addresses */
      branchCount = 0;
      for (scan = funcStart; scan < funcEnd; ++scan) {
         if (mode == IntraFunc)
            while (scan < funcEnd && is_const(scan)) ++scan;

         if ((*scan & filter) == modeMask) {
            /* add branch to table */
            permutation[branchCount] = branchCount;
            branchAddr[branchCount] = (scan - funcBegin);
            tmp = (*scan & 0x00ffffff) |
                  ((*scan & 0x00800000) ? 0xff000000 : 0);
            if (mode == IntraFunc) {
               // advance branch target to non-NOP
               int *active = scan + 2 + tmp;
               while (*active == NOP) { ++active; ++tmp; }
               *scan = (*scan & 0xff000000) | (tmp & 0x00ffffff);
            }
            branchTarget[branchCount] = (scan - funcBegin) + 2 + tmp;
            ++branchCount;
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
      int align;
      int nopRun = 0;
      currAddr = 0;
      currTarget = 0;
      offset = 0;

      packed = funcBegin;
      for (scan = funcBegin; scan <= funcEnd; ++scan, ++offset) {
         if (mode == InterFunc) {
            if (*scan == NOP) {
               *packed++ = NOP;
               if (++nopRun < 8) continue;
               // 8 consecutive nops means interfunc region found
               int *last = scan - 8; // last non-NOP instruction
               int *scan2 = scan + 1;
               packed -= 8;
               while (scan2 <= funcEnd && *scan2 == NOP) {
                  ++scan2;
                  ++offset;
               }
               if (scan2 > funcEnd)
                  goto finalize_relocate;

               int padCount;
               int alignHead = (last - cbegin) & 3;
               int alignTail = (scan2 - cbegin) & 3;
               if (alignTail > alignHead) { // elide
                  padCount = ((alignTail - alignHead) - 1);
                  while (padCount-- > 0)
                     *packed++ = NOP;
               }
               else if (!(alignTail == 0 && alignHead == 3)) { // pad
                  padCount = 3 - (alignHead - alignTail);
                  while (padCount-- > 0)
                     *packed++ = NOP;
               }
               scan = scan2 - 1;
               nopRun = 0;
               continue;
            }
            else
               nopRun = 0;
         }

         align = 1;
         while (currTarget < branchCount) {
            if (branchTarget[currTarget] == offset) {
               if (align && mode == IntraFunc &&
                   branchAddr[permutation[currTarget]] > offset &&
                   (*scan & 0xfff00000) != 0xe1500000 && // cmp
                   (*scan & 0xfff00000) != 0xe3500000) {
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
         if (mode == InterFunc) {
            if (*scan != NOP) *packed++ = *scan;
         }
         else if (is_const(scan)) {
            if (scan != packed) {
               tmp = find_const((scan-cbegin)*4);
               int iaddr = cnst_pool[tmp].data_addr/4;
               cnst_bit[iaddr/(sizeof(int)*8)] &=
                     ~(1 << (iaddr & ((sizeof(int)*8)-1)));
               iaddr = (packed-cbegin);
               cremap[tmp-lowc].data_addr = iaddr*4;
               cnst_bit[iaddr/(sizeof(int)*8)] |=
                     (1 << (iaddr & ((sizeof(int)*8)-1)));
               *packed++ = *scan;
            }
         }
         else if (*scan != NOP) {
            if ((*scan & 0xffff0000) == 0xe59f0000 || // ldr  rN, [pc, #X]
                (*scan & 0xffbf0f00) == 0xed9f0a00) { // vldr sN, [pc, #X]
               if (((*scan & (1<<23))) && scan != packed) {
                  // don't remap FP consts moved by func rename_register1()
                  int is_vldr = ((*scan & 0xffbf0f00) == 0xed9f0a00); // vldr
                  int oset = is_vldr ? (*scan & 0xff) : ((*scan & 0xfff) / 4);
                  tmp = find_const(((scan + 2 + oset) - cbegin)*4);

                  for (inst = &cremap[tmp-lowc].inst;
                       *inst != 0; inst = &(*inst)->next);
                  *inst = (struct ia_s *) malloc(sizeof(struct ia_s));
                  (*inst)->inst_addr = (packed-cbegin)*4;
                  (*inst)->next = 0;
                  *packed++ = *scan;
               }
            }
            else if ((*scan & 0x0e000000) == 0x0a000000) {
               if (*scan & (1<<24)) {
                  // adjust bl instruction address for IntraFunc mode
                  *scan += scan - packed;
               }
               *packed++ = *scan;
            }
            else
               *packed++ = *scan;
         }
      }

finalize_relocate:

      /* fixup branch instructions with new target address */
      for (ii = 0; ii < branchCount; ++ii) {
         tmp = branchAddr[permutation[ii]];
         funcBegin[tmp] = (funcBegin[tmp] & 0xff000000) |
                      ((branchTarget[ii] - tmp - 2) & 0x00ffffff);
      }

      if (mode == InterFunc) { // for ELF alignment
         while ((int) packed & 0x0f)
            *packed++ = NOP;
      }

      retVal = packed ; // pointer just past end of code

      while (packed <= funcEnd)
         *packed++ = NOP;

      // update const_pool load operations
      if (mode == IntraFunc && cremap_size) {
         for (ii=0; ii < cremap_size; ++ii) {
            tmp = cremap[ii].data_addr / 4;
            for (inst = &cremap[ii].inst; *inst != 0; inst = &(*inst)->next) {
               scan = &cbegin[(*inst)->inst_addr/4];
               if ((*scan & 0xffbf0f00) == 0xed9f0a00) { // vldr sd, [pc, #X]
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

// Basic integer push/pop optimizations (often memory addresses)
static void create_pushpop_map(int *instInfo, int *funcBegin, int *funcEnd)
{
   int i, lev = 0;
   int maxlev = 0;
   int *scanm1, *scanm2, *scanp1;
   int *stack[10];
   int *scan;
   int np = 0;

   create_inst_info(instInfo, funcBegin, funcEnd);
   create_bb_info(instInfo, funcBegin, funcEnd);

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
         if (scan != stack[lev]+1) {
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
   }

   if (lev != 0) {
      printf("bug: optimizer push/pop lev = %d\n", lev);
      exit(-1);
   }

   // innermost to outermost push/pop pairs
   for (i = 0; i < np; ++i) {
      scanm1 = active_inst(pair[i].push, -1); // inst before push
      if ((*scanm1 & 0x0e000000) != 0x0a000000 &&
          (instInfo[pair[i].push-funcBegin] & RI_bb)) continue;
      scanp1 = active_inst(pair[i].pop,   1); // inst after pop
      if ((*scanp1 & 0xffbfffff) == 0xe5810000 || // str[b] r0, [r1]
          *scanp1 == 0xed810a00) {                // vstr s0, [r1]
         int *pup1 = active_inst(pair[i].push, 1);
         int cassign = (*pup1 == 0xed900a00);  // vldr s0, [r0]
         int *pushp1 = &instInfo[pup1-funcBegin];
         int *r0d = cassign ? pushp1 : find_def(instInfo, pushp1, 0, S_FWD);
         int *r0u = find_use(instInfo, pushp1, 0, S_FWD);
         if (r0u == 0) r0u = r0d+1;
         if ((&funcBegin[r0d-instInfo] > scanp1 || r0d <= r0u) &&
             ((*scanm1 & 0xfffff000) == 0xe28b0000 ||  // add r0, fp, #X
              (*scanm1 & 0xfffff000) == 0xe24b0000 )) { // sub r0, fp, #X
            int off = arith_off(*scanm1);
            if (*scanp1 == 0xed810a00) { // vstr
               off /= 4;
               if (off >= 256) continue;
            }
            else { // str[b]
               if (off >= 4096) continue;
            }
            if ((*scanm1 & 0xfffff000) == 0xe2800000) // add
               off |= 1 << 23;
            if (&funcBegin[r0d-instInfo] < scanp1 && r0u == r0d) {
               if (funcBegin[r0d-instInfo] == 0xe5900000 || //  ldr r0, [r0]
                   funcBegin[r0d-instInfo] == 0xed900a00) { // vldr s0, [r0]
                  funcBegin[r0d-instInfo] =
                     (funcBegin[r0d-instInfo] & 0xff70ff00) | 0x000b0000 | off;
                  *r0d |= 0xb0000; // rn = fp
               }
               else {
                  continue;
               }
            }
            // default case: pushed reg not used before pop
            *scanm1 = NOP;
            instInfo[scanm1-funcBegin] = 0; // &= RI_bb;
            *pair[i].push = NOP;
            instInfo[pair[i].push-funcBegin] = 0; // &= RI_bb;
            *pair[i].pop  = NOP;
            instInfo[pair[i].pop-funcBegin] = 0; // &= RI_bb;
            *scanp1 = (*scanp1 & 0xff70ff00) | 0x000b0000 | off;
            instInfo[scanp1-funcBegin] |= 0xb0000; // rn = 1 | b == b
         }
         else if (r0d < r0u &&
                  (*scanm1 & 0xffff0000) == 0xe59f0000 && // ldr r0, [pc, #X]
                  (scanm1 + 2 + (*scanm1 & 0xfff)/4) > pair[i].pop) {
            rel_pc_ldr(pair[i].pop, scanm1);
            *pair[i].pop |= 0x1000;
            instInfo[pair[i].pop-funcBegin] =
               instInfo[scanm1-funcBegin] | 0x1000;
            *scanm1 = NOP;
            instInfo[scanm1-funcBegin] = 0; // &= RI_bb;
            *pair[i].push = NOP;
            instInfo[pair[i].push-funcBegin] = 0; // &= RI_bb;
         }
         else if (r0d < r0u &&
                 ((*scanm1 & 0xfffff000) == 0xe2800000 ||  // add r0, r0, #X
                  (*scanm1 & 0xfffff000) == 0xe2400000)) { // sub r0, r0, #X
            int off = arith_off(*scanm1);
            if (*scanp1 == 0xed810a00) { // vstr
               off /= 4;
               if (off >= 256) continue;
            }
            else { // str[b]
               if (off >= 4096) continue;
            }
            if ((*scanm1 & 0xfffff000) == 0xe2800000) // add
               off |= 1 << 23;

            scanm2 = active_inst(scanm1,-1);
            if (*scanm2 == 0xe5900000) {  // ldr r0, [r0]
               int *scan2 = pair[i].push + 1;
               for (; scan2 < pair[i].pop; ++scan2) {
                  if (*scan2 == NOP11 && !is_const(scan2)) break;
               }
               if (scan2 != pair[i].pop) continue;
               r0d = find_def(instInfo, pushp1, 2, S_FWD);
               r0u = find_use(instInfo, pushp1, 2, S_FWD);
               if ((r0u == 0 || r0u > pair[i].pop) &&
                   (r0d == 0 || r0d > pair[i].pop)) {
                  *scanm2 = *scanm2 | (2 << 12);
                  instInfo[scanm2-funcBegin] |= 2<<12;
                  *scanp1 = (*scanp1 & 0xff70ff00) | 0x00020000 | off;
                  instInfo[scanp1-funcBegin] += 0x10000;
                  *scanm1 = NOP;
                  instInfo[scanm1-funcBegin] = 0; // &= RI_bb;
                  *pair[i].push = NOP;
                  instInfo[pair[i].push-funcBegin] = 0; // &= RI_bb;
                  *pair[i].pop  = NOP;
                  instInfo[pair[i].pop-funcBegin] = 0; // &= RI_bb;
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
   create_bb_info(instInfo, funcBegin, funcEnd);
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
         if (scan != stack[lev]+1) {
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
            scanm1 = active_inst(pair[i].push, -1); // inst before push
            if ((*scanm1 & 0x0e000000) != 0x0a000000 &&
                (instInfo[pair[i].push-funcBegin] & RI_bb)) continue;
            int *pop  = &instInfo[pair[i].pop-funcBegin];
            int *pup1 = active_inst(pair[i].push, 1);
            int *pushp1 = &instInfo[pup1-funcBegin];
            int *r1push1u = find_use(instInfo, pushp1, 1, S_FWD);
            int *r1push1d = find_def(instInfo, pushp1, 1, S_FWD);
            for (scan = pair[i].push + 1; scan < pair[i].pop; ++scan) {
               if (*scan == NOP11 && !is_const(scan)) break; // func call
            }
            if (scan != pair[i].pop) continue; // skip regions with func call

            /* if r1 not used or defined between push and pop... */
            if (r1push1d == pop && r1push1u > pop) {
               int *m1 = &instInfo[scanm1-funcBegin];
               int *r0md = find_def(instInfo, m1, 0, S_FWD);
               int *r0push1u = find_use(instInfo, pushp1, 0, S_FWD);
               int *r0push1d = find_def(instInfo, pushp1, 0, S_FWD);
               if (r0push1d == 0) r0push1d = pop;
               int m1modifiable =
                  (*(pair[i].push - 1) != NOP13) && (r0md == m1);

               /* if r0 defined in instruction before push and */
               /* within push/pop, def of r0 happens before use... */
               if (m1modifiable && r0push1u > r0push1d) {
                  if ((*scanm1 & 0xf0000000) == 0xe0000000) { // uncond op
                     int dstReg =
                        (((*scanm1 & 0x0e0000f0) == 0x90 ||
                          (*scanm1 & 0x0ff000f0) == 0x07500010) ?
                        (1<<16) : (1<<12));
                     *scanm1 |= dstReg;
                     *m1 |= dstReg;
                     *pair[i].push = NOP;
                      instInfo[pair[i].push-funcBegin] = 0; // &= RI_bb;
                     *pair[i].pop  = NOP;
                      instInfo[pair[i].pop-funcBegin] = 0; // &= RI_bb;
                  }
               }
               else {
                  *pair[i].push = 0xe1a01000; // mov r1, r0
                  instInfo[pair[i].push-funcBegin] =
                     RI_RdAct | RI_RdDest | RI_RmAct | (1 << 12);
                  *pair[i].pop  = NOP;
                  instInfo[pair[i].pop-funcBegin] = 0; // &= RI_bb;
               }
            }
         }
      }
   }
}

// Basic FP push/pop optimizations
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
         if (scan != stack[lev]+1) {
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
   }

   if (lev != 0) {
      printf("bug: optimizer push/pop lev = %d\n", lev);
      exit(-1);
   }

   for (i = 0; i < np; ++i) {
      int *pop = pair[i].pop;
      for (scan = pair[i].push + 1; scan < pop; ++scan) {
         if (*scan == NOP11 && !is_const(scan)) break; // func call
      }
      if (scan != pop) continue; // skip regions with func call

           pop  = &instInfo[pop-funcBegin];
      int *pushp1 = &instInfo[(pair[i].push-funcBegin)+1];
      int *r1push1u = find_use(instInfo, pushp1, 1, S_FWD);
      int *r1push1d = find_def(instInfo, pushp1, 1, S_FWD);

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

// pointer optimizations, mirroring apply_peepholes7
static void create_pushpop_map2b(int *instInfo, int *funcBegin, int *funcEnd)
{
   int isFP, isAdd, isSub, lev = 0;
   int *stack[10];
   int *scan;
   int np = 0;
   int *safeEnd = funcEnd - 6;

   create_inst_info(instInfo, funcBegin, funcEnd);

   for (scan = funcBegin; scan < safeEnd; ++scan) {
      scan = skip_nop(scan, S_FWD);

      if ( (*scan & 0xffff0fff) == 0xe52d0004 && // push {rx}
          *(scan-1) != NOP1 &&
          *(scan+3) != 0xe3a0780f && // mov r7, #983040
          *(scan+6) != 0xe3a0780f) { // mc specific hack
         stack[lev] = scan;
         ++lev;
      }
      else if (*scan == 0xe49d1004 && // pop {r1}
               *(scan-1) != NOP1) {
         --lev;
         if (scan != stack[lev]+1) {
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

   for (int i = 0; i < np; ++i) {
      int *popp1 = active_inst(pair[i].pop, 1);
      if (!(((isFP = (*popp1 & 0xff200f00) == 0xed000a00) // vldr|str
              && ((*popp1 & 0xff) == 0))           ||     // zero offset
            (*popp1 & 0xff200000) == 0xe5000000))         // (ldr|str)[b]
         continue;

      int *pop = pair[i].pop;
      for (scan = pair[i].push + 1; scan < pop; ++scan) {
         if (*scan == NOP11 && !is_const(scan)) break; // func call
      }
      if (scan != pop) continue; // skip regions with func call

      pop = &instInfo[pop-funcBegin];
      int *scanm1 = active_inst(pair[i].push, -1);
      int inst = *scanm1;
      int *scanm2 = active_inst(scanm1,-1);
      int inst2 = *scanm2;
      if (((isAdd = (inst & 0xfff00000) == 0xe2800000) || // add #X
          (inst & 0xfff00000) == 0xe2400000) &&           // sub #X
          (inst & RI_Rd) == (*pair[i].push & RI_Rd)) {
         if ((instInfo[scanm2-funcBegin] & RI_memOp) == RI_frameW &&
             (inst & RI_Rd) == (*scanm2 & RI_Rd)) continue;
         int reg = (*pair[i].push & RI_Rn) >> 16;
         int *pushp1 = &instInfo[(pair[i].push-funcBegin)+1];
         int *r1push1u = find_use(instInfo, pushp1, reg, S_FWD);
         int *r1push1d = find_def(instInfo, pushp1, reg, S_FWD);
         if (r1push1d == 0) r1push1d = &instInfo[funcEnd-funcBegin];

         /* if rx not used or defined between push and pop... */
         if (r1push1d > pop && (!r1push1u || r1push1u >= r1push1d)) {
            int off = isFP ? ((*popp1 & 0xff) * 4) : (*popp1 & 0xfff);
            if ((inst & RI_Rn) == ((inst & RI_Rd) << 4) &&
                (inst & RI_Rd) == (reg << 12)) { // pre inc/dec
               if (off == 0) {
                  if (isFP) {
                     if (isAdd)
                        *popp1 = (*popp1 & ~RI_Rn) | (inst & RI_Rn);
                     else {
                        *popp1 = (*popp1 & 0xff70ff00) |
                                 (inst & RI_Rn) | (1 << 21) | 1;
                        *scanm1 = NOP;
                     }

                  }
                  else {
                     *popp1 = (*popp1 & 0xff70f000) | (inst & RI_Rn) |
                             ((isAdd ? 5 : 1) << 21) | arith_off(inst);
                     *scanm1 = NOP;
                  }
                  *pair[i].pop = NOP; *pair[i].push = NOP;
               }
               else if (off * ((*popp1 & (1 << 23)) ? 1 : -1) +
                        arith_off(inst) * (isAdd ? 1 : -1) == 0) { // post
                  if (!isFP || isAdd) {
                     *popp1 = (*popp1 & ~RI_Rn) |
                              (inst & RI_Rn) | 3 << 23;  // post inc/dec
                     if (isFP) *popp1 |= (1 << 21);
                     *scanm1 = NOP;
                     *pair[i].pop = NOP; *pair[i].push = NOP;
                  }
               }
            }
            else if (off == 0) { // post inc/dec
               if ((isSub = (inst2 & 0xfff00000) == 0xe2400000) || // sub #X
                   (inst2 & 0xfff00000) == 0xe2800000) {           // add #X
                  if ((inst2 & RI_Rn) == (inst & RI_Rn) &&
                      (inst2 & RI_Rn) == ((inst2 & RI_Rd) << 4) &&
                      (inst & 0xfff) == (inst2 & 0xfff) && isSub == isAdd) {
                     if (isFP) {
                        if (isSub) {
                           *popp1 = (*popp1 & ~RI_Rn) |
                                    (*scanm2 & RI_Rn) | 0x00800001;
                           *scanm1 = NOP;
                        }
                        else {
                           *popp1 = (*popp1 & 0xfe70ff00) | (inst & RI_Rn) |
                                    ((isAdd ? 0 : 1) << 23) | (1 << 21) | 1;
                           *scanm2 = *scanm1 = NOP;
                        }
                     }
                     else {
                        *popp1 = (*popp1 & 0xfe70f000) | (inst & 0x000f0fff) |
                                 ((isAdd ? 0 : 1) << 23);
                        *scanm2 = *scanm1 = NOP;
                     }
                     *pair[i].pop = NOP; *pair[i].push = NOP;
                  }
               }
            }
         }
      }
   }
}

// Register renaming push/pop optimizations (both integer and FP)
static int create_pushpop_map3(int *instInfo, int *funcBegin, int *funcEnd,
                               int hasFuncCall, int base, int dofloat)
{
   int *scan, *scan2;
   int *scanm1, *scanp1;
   int i, guard, rd, rn;
   int *stack[10];
   int lev = 0;
   int np = 0;
   int retBase = 0;
   int *rxu, *rxd, *rdu, *rdd, *rnu, *rnd, *rfinal;

   int br = avail_reg(base);
   if (br == -1)
      return base;

   if (dofloat)
      create_inst_info_f(instInfo, funcBegin, funcEnd);
   else
      create_inst_info(instInfo, funcBegin, funcEnd);

   create_bb_info(instInfo, funcBegin, funcEnd);

   for (scan = funcBegin; scan < funcEnd; ++scan) {
      scan = skip_nop(scan, S_FWD);

      guard = dofloat ? (*scan == 0xed2d0a01) : // vpush {s0}
              ((*scan & 0xffff0fff) == 0xe52d0004 && // push {r[0-10]}
               ((1 << ((*scan & RI_Rd) >> 12)) & UREG_MASK));

      if (guard &&
          *(scan-1) != NOP1 &&
          *(scan+3) != 0xe3a0780f && // mov r7, #983040
          *(scan+6) != 0xe3a0780f) { // mc specific hack
         stack[lev] = scan;
         ++lev;
      }
      else {
         guard = dofloat ? (*scan == 0xecfd0a01) : // vpop {s1}
                 ((*scan & 0xffff0fff) == 0xe49d0004 && // pop {r[0-10]}
                  ((1 << ((*scan & RI_Rd) >> 12)) & UREG_MASK));


         if (guard && *(scan-1) != NOP1) {
            --lev;
            if (scan != stack[lev]+1) {
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
   }

   if (lev != 0) {
      printf("bug: optimizer push/pop lev = %d\n", lev);
      exit(-1);
   }

   // innermost to outermost push/pop pairs
   for (i = 0; i < np; ++i) {

#ifdef OLD_CLEANUP_XFORM
      if ((*scanp1 & 0xffbfffff) == 0xe5810000 || // str[b] r0, [r1]
          *scanp1 == 0xed810a00) {                // vstr s0, [r1]
         if (r0d < r0u && (instInfo[pair[i].push-funcBegin] & RI_bb) == 0 &&
            (*scanm1 & 0xff7ff000) == 0xe51b0000) { // ldr r0, [fp, #X]
            *pair[i].pop = *scanm1 | (1<<12);
            *scanm1 = NOP;
            *pair[i].push = NOP;
         }
      }
#endif

      for (scan = pair[i].push + 1; scan < pair[i].pop; ++scan) {
         if (*scan == NOP11 && !is_const(scan)) break; // func call
      }
      if (scan != pair[i].pop) continue; // skip regions with func call

      scanm1 = active_inst(pair[i].push,-1);
      scanp1 = active_inst(pair[i].pop,  1);

      if (dofloat && (*scanm1 & 0xfff0f050) == 0xeeb00040) { // vmov s0, Fm
         rn = (*scanm1 & RI_Rm)*2 + ((*scanm1 & 0x20) >> 5);
         if (rn >= 2 && rn < br) {
            *scanm1 = NOP;
            *pair[i].push = NOP;
            *pair[i].pop = NOP;
            instInfo[scanm1-funcBegin] &= RI_bb;
            instInfo[pair[i].push-funcBegin] &= RI_bb;
            instInfo[pair[i].pop-funcBegin]  &= RI_bb;
            reg_rename_f(rn, 1, &instInfo[scanp1-funcBegin], scanp1);
            continue;
         }
      }

      int info = instInfo[scanm1-funcBegin];
      if ((info & RI_hasD) == 0) continue;

      rd = dofloat ? (((info & RI_Rd) >> 12) | ((info & RI_Sd) >> 18)) :
           ((info & RI_RdDest) ?
            ((info & RI_Rd) >> 12) : ((info & RI_Rn) >> 16));

      if (rd != (dofloat ? 0 : ((*pair[i].push & RI_Rd) >> 12))) continue;

      if ((instInfo[pair[i].push-funcBegin] & RI_bb) ||
          (instInfo[scanp1-funcBegin] & RI_bb)) {
         continue;
      }

      rn = dofloat ? 1 : ((*pair[i].pop & RI_Rd) >> 12);
      rnd = find_def(instInfo, &instInfo[scanp1-funcBegin], rn, S_FWD);
      if (rnd == 0) rnd = &instInfo[funcEnd-funcBegin];
      rnu = find_use(instInfo, &instInfo[scanp1-funcBegin], rn, S_FWD);
      rfinal = find_use(instInfo, rnd, rn, S_BACK);

      for (scan = &instInfo[scanp1-funcBegin]; scan <= rfinal; ++scan) {
         if (*scan & RI_bb) break;
      }
      if (scan <= rfinal) continue;

      int reg, *rscan;
      int *pushp1 = &instInfo[pair[i].push-funcBegin]+1;
      int tbase = base;
      for (reg = avail_reg(tbase); reg != -1;
           reg = avail_reg(tbase ^= (1 << reg))) {

         if (reg <= rd) continue;

         if (reg == 14 && hasFuncCall && !dofloat) break; // skip link register

         rxd = find_def(instInfo, pushp1, reg, S_FWD);
         rxu = find_use(instInfo, pushp1, reg, S_FWD);
         if (rxd != 0 && rxd <  rfinal) continue;
         if (rxu != 0 && rxu <= rfinal) continue;

         rdu = find_use(instInfo, pushp1, rd, S_FWD);
         rdd = find_def(instInfo, pushp1, rd, S_FWD);
         if (rdd == 0) rdd = &instInfo[pair[i].pop-funcBegin];
         if (rdu && rdu <= rdd) {
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

         if (retBase == 0 || popcount32b(tbase) < popcount32b(retBase))
            retBase = tbase;

         if (dofloat) {
            *scanm1 = (*scanm1 & ~(RI_Rd | RI_Sd)) |
                      ((reg & 0x1e) << 11) | ((reg & 1) << 22);
            instInfo[scanm1-funcBegin] = (info & ~(RI_Rd | RI_Sd)) |
               ((reg & 0x0f) << 12) | ((reg & 0x10) << 18);
         }
         else {
            if (((*scanm1 & 0x0e0000f0) == 0x90) ||
                 (*scanm1 & 0x0ff000f0) == 0x07500010) {
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
         instInfo[pair[i].push-funcBegin] &= RI_bb;
         instInfo[pair[i].pop-funcBegin]  &= RI_bb;

         break;
      }
   }

   // very few push/pop pairs remain now
   if (!dofloat) {
      for (i = 0; i < np; ++i) {
         scan2 = pair[i].push;
         if (*scan2 == NOP) continue;

         rd = (*scan2 & RI_Rd)>>12;
         if (rd < 2 || rd >= br) continue;

         for (scan = scan2 + 1; scan < pair[i].pop; ++scan) {
            if (*scan == NOP11 && !is_const(scan)) break; // func call
         }
         if (scan != pair[i].pop) continue; // skip regions with func call

         rdd = find_def(instInfo, &instInfo[scan2-funcBegin]+1, rd, S_FWD);
         scan2 = pair[i].pop;
         rn = (*scan2 & RI_Rd)>>12;
         rnu = find_use(instInfo, &instInfo[scan2-funcBegin]+1, rn, S_FWD);
         rnd = find_def(instInfo, &instInfo[scan2-funcBegin]+1, rn, S_FWD);
         if (rnd == 0)
            rnd = find_use(instInfo, &instInfo[funcEnd-funcBegin]-1,
                           rn, S_BACK);

         if (rdd && rdd < rnd) continue;
         // rename all uses of rn <= rnd to rd
         while (rnu && rnu <= rnd) {
            reg_rename(rd, rn, rnu, &funcBegin[rnu-instInfo]);
            rnu = find_use(instInfo, rnu+1, rn, S_FWD);
         }
         *pair[i].push = NOP;
         *pair[i].pop  = NOP;
         instInfo[pair[i].push-funcBegin] &= RI_bb;
         instInfo[pair[i].pop-funcBegin]  &= RI_bb;
      }
   }

   return (retBase == 0) ? base : (retBase ^ (1 << avail_reg(retBase)));
}

// get rid of spurious push/pops which remain after advanced optimization
static void create_pushpop_map4(int *instInfo, int *funcBegin, int *funcEnd,
                                int dofloat)
{
   int guard;
   int i, lev = 0;
   int *stack[10];
   int *scan;
   int np = 0;
   int *safeEnd = funcEnd - 6;

   if (dofloat)
      create_inst_info_f(instInfo, funcBegin, funcEnd);
   else
      create_inst_info(instInfo, funcBegin, funcEnd);

   for (scan = funcBegin; scan < safeEnd; ++scan) {
      scan = skip_nop(scan, S_FWD);

      guard = dofloat ?
              ((*scan & 0xffbf0fff) == 0xed2d0a01) : // vpush {sN}
              ((*scan & 0xffff0fff) == 0xe52d0004 && // push {r[0-10]}
               ((1 << ((*scan & RI_Rd) >> 12)) & UREG_MASK));

      if (guard &&
          *(scan-1) != NOP1 &&
          *(scan+3) != 0xe3a0780f && // mov r7, #983040
          *(scan+6) != 0xe3a0780f) { // mc specific hack
         stack[lev] = scan;
         ++lev;
      }
      else {
         guard = dofloat ? (*scan == 0xecfd0a01) : // vpop {s1}
                 ((*scan & 0xffff0fff) == 0xe49d0004 && // pop {r[0-10]}
                  ((1 << ((*scan & RI_Rd) >> 12)) & UREG_MASK));


         if (guard && *(scan-1) != NOP1) {
            --lev;
            if (scan != stack[lev]+1) {
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
   }

   if (lev != 0) {
      printf("bug: optimizer push/pop lev = %d\n", lev);
      exit(-1);
   }

   for (i = 0; i < np; ++i) {
      int *pop = pair[i].pop;
      for (scan = pair[i].push + 1; scan < pop; ++scan) {
         if (*scan == NOP11 && !is_const(scan)) break; // func call
      }
      if (scan != pop) continue; // skip regions with func call

      pop  = &instInfo[pop-funcBegin];

      int reg = dofloat ?
                (((*pair[i].push & RI_Rd) >> 11) |
                 ((*pair[i].push & RI_Sd) ? 1 : 0)) :
                 ((*pair[i].push & RI_Rd) >> 12);

      int *pushp1 = &instInfo[(pair[i].push-funcBegin)+1];
      int *rxpush1d = find_def(instInfo, pushp1, reg, S_FWD);

      /* if reg not re-defined between push and pop... */
      if (!rxpush1d || rxpush1d > pop) {

         int popr = dofloat ? 1 : ((*pair[i].pop & RI_Rd) >> 12);
         pushp1 = &instInfo[(pair[i].pop-funcBegin)+1];
         int *r1push1d = find_def(instInfo, pushp1, popr, S_FWD);
         if (r1push1d == 0) r1push1d = &instInfo[funcEnd-funcBegin];
         int *r1push1u = find_use(instInfo, r1push1d, popr, S_BACK);
         rxpush1d = find_def(instInfo, pushp1, reg, S_FWD);
         if (rxpush1d == 0) rxpush1d = &instInfo[funcEnd-funcBegin];

         if (r1push1u > rxpush1d) {
            printf("code generation error\n");
            exit(-1);
         }

         r1push1d = r1push1u;
         r1push1u = find_use(instInfo, pushp1, popr, S_FWD);
         do {
            if (dofloat)
               reg_rename_f(reg, popr, r1push1u,
                            &funcBegin[r1push1u-instInfo]);
            else
               reg_rename(reg, popr, r1push1u,
                            &funcBegin[r1push1u-instInfo]);

            r1push1u = find_use(instInfo, r1push1u+1, popr, S_FWD);
         } while (r1push1u && r1push1u <= r1push1d);

         *pair[i].push = NOP;
         *pair[i].pop = NOP;
      }
   }
}

#if 0
/* simple functions with no locals do not need a frame */
static void simplify_frame(int *funcBegin, int *funcEnd)
{
   int *scan, *scanp1, fp;

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
            else
               fo -= 4;
         }
         else if ((*scan & 0xfffff000) == 0xe28dd000) { // add sp, sp, #X
            // rotate field will be >= 4 if present
            fo -= (((*scan & 0xf00) == 0) ? (*scan & 0xff) :
                   ((*scan & 0xff) << (32 - ((*scan & 0xf00) >> 8)*2)));
         }
         else if ((*scan & 0xffff000f) == 0xe52d0004 || // push {rX}
                  (*scan & 0xffff0f0f) == 0xed2d0a01) { // vpush {sX}
            fo += 4;
         }
         else if ((*scan & 0xffff000f) == 0xe49d0004 || // pop {rX}
                  (*scan & 0xffff0f0f) == 0xecbd0a01) { // vpop {sX}
            fo -= 4;
         }
         else if ((fp = (*scan & 0xff2f0f00) == 0xed0b0a00) || // vldr | vstr
                  (*scan & 0xff2f0000) == 0xe50b0000) {     // str/ldr [fp,#Y]
            int offmask = fp ? 0xff : 0xfff;
            int off = (*scan & offmask) * ((*scan & (1<<23)) ? 1 : -1) +
                      fo / (fp ? 4 : 1);
            int add = (off < 0) ? 0 : (1 << 23);
            if (off < 0) off = -off;
            *scan = (*scan & (0xff7fffff ^ offmask)) +
                    0x20000 + add + off; // fp -> sp
            if (off > offmask) { printf("optimizer error\n"); exit(-1); }
         }
      }
   }
}
#endif

/**********************************************************/
/****       convert frame vars to registers          ******/
/**********************************************************/

#define REN_BUF 128

// hack to support nonmutable floating point I-stream constants
// will likely need full dependency analysis later
static int rename_register1(int *instInfo, int *funcBegin, int *funcEnd,
                            int fbase_)
{
#define MAX_FP_CONST 6
   int i, done, tmp, numReg = 0;
   int fbase = fbase_;
   int *scan, *scanp1, *scanfp;
   int fpcnst[REN_BUF];
   int count[REN_BUF];
   int FPreg[MAX_FP_CONST];

   // avoid tricky logic surrounding stack frame optimizations
   // if (funcBegin[2] == NOP) return fbase;

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

   if (numReg == 0) return fbase;

   // Up to six FP constants for now
   if (numReg > MAX_FP_CONST)
      numReg = MAX_FP_CONST;

   for (scan = funcBegin; *scan != NOP; ++scan);
   for (i=0; i<numReg; ++i) {
      if (*scan != NOP) {
         printf("out of register1 assignment space\n");
         exit(-1);
      }
      fbase ^= (1 << (FPreg[i] = avail_reg(fbase)));
      if (count[i] & 0x40000000) { // global const
         if (scan[1] != NOP) {
            printf("out of register1 assignment space\n");
            exit(-1);
         }
         // ldr r0, [pc, #X]
         *scan = 0xe51f0000 | (((scan + 2) - (funcBegin-(i+1)))*4);
         scan[1] = 0xed900a00 | ((FPreg[i] & 0x1e) << 11) |
                   ((FPreg[i] & 1) << 22); // vldr s(FPreg), [r0]
         ++scan;
      }
      else { // local const
         *scan = 0xed1f0a00 | ((FPreg[i] & 0x1e) << 11) | // vldr
                 ((FPreg[i] & 1) << 22) |
                 ((scan + 2) - (funcBegin-(i+1)));
      }
      *(funcBegin - (i+1)) = fpcnst[i];
      ++scan;
   }

   // Note: Likely need block boundary check on some of these...

   for (scan = funcBegin; scan <= funcEnd; ++scan) {
      scan = skip_nop(scan, S_FWD);

      if ((*scan & 0xffbf0f00) == 0xed9f0a00) { // vldr sN, [pc, #x]
         tmp = *(scan + 2 + (*scan & 0xff));
         for (i = 0; i < numReg; ++i)  {
            if (fpcnst[i] == tmp) {
               delete_const(scan + 2 + (*scan & 0xff), scan);
               scanp1 = active_inst(scan, 1);
               if ((*scanp1 & 0xff70ff00) == 0xed000a00) { // vstr s0, ...
                  *scanp1 |= ((FPreg[i] & 0x1e) << 11) |
                             ((FPreg[i] & 1) << 22);
                  *scan = NOP;
               }
               else {
                  if (*scanp1 == 0xed2d0a01) { // vpush s0
                     *scan = 0xeeb00a40 | (*scan & 0x0040f000) | // vmov
                             (FPreg[i] >> 1) | ((FPreg[i] & 1) << 5);
                  }
                  else if (*scanp1 == 0xecfd0a01) { // vpop s1
                     scanp1 = active_inst(scanp1, 1);

                     create_inst_info_f(instInfo, scanp1, scanp1);
                     reg_rename_f(FPreg[i], 0, instInfo, scanp1);
                     *scan = NOP;
                  }
                  else if ((*scanp1 & 0xffffff00) == 0xe59f1000) {
                     // ldr r1, [pc, #x]
                     scanp1 = active_inst(scanp1, 1);
                     if ((*scanp1 & 0xffffff00) == 0xed810a00) {
                        // vstr s0, [r1, #x]
                        *scanp1 |= ((FPreg[i] & 0x1e) << 11) |
                                   ((FPreg[i] & 1) << 22);
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
            for (i = 0; i < numReg; ++i)  {
               if (fpcnst[i] == tmp) {
                  delete_const(scan + 2 + (*scan & 0xfff)/4, scan);
                  scanp1 = active_inst(scanfp, 1);
                  if ((*scanp1 & 0xff70ff00) == 0xed000a00) { // vstr s0, ...
                     *scanp1 |= ((FPreg[i] & 0x1e) << 11) |
                                ((FPreg[i] & 1) << 22);
                     *scan = NOP;
                     *scanfp = NOP;
                  }
                  else {
                     if (*scanp1 == 0xed2d0a01) { // vpush s0
                        *scan = 0xeeb00a40 | (*scan & 0x0040f000) | // vmov
                                (FPreg[i] >> 1) | ((FPreg[i] & 1) << 5);
                        *scanfp = NOP;
                     }
                     else if (*scanp1 == 0xecfd0a01) { // vpop s1
                        scanp1 = active_inst(scanp1, 1);
                        create_inst_info_f(instInfo, scanp1, scanp1);
                        reg_rename_f(FPreg[i], 0, instInfo, scanp1);
                        *scan = NOP;
                        *scanfp = NOP;
                     }
                     else if ((*scanp1 & 0xffffff00) == 0xe59f1000) {
                        // ldr r1, [pc, #x]
                        scanp1 = active_inst(scanp1, 1);
                        if ((*scanp1 & 0xffffff00) == 0xed810a00) {
                           // vstr s0, [r1, #x]
                           *scanp1 |= ((FPreg[i] & 0x1e) << 11) |
                                      ((FPreg[i] & 1) << 22);
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

   return fbase;
}

// create rudimentary loop level information.
static int find_loop(int **loopRec_, int *funcBegin, int *funcEnd)
{
   int numLoop = 0;
   int **loopRec = loopRec_;
   for (int *scan=funcBegin; scan < funcEnd; ++scan)
   {
      if ((*scan & 0x0f800000) == 0x0a800000 &&
          (*scan & 0x00ffffff) != 0x00ffffff) {
         if (is_const(scan)) continue;
         int tmp = *scan | 0xff000000;
         *loopRec++ = scan + 2 + tmp;
         *loopRec++ = scan;
         ++numLoop;
      }
   }
   return numLoop;
}

static int in_loop(int **loopRec, int numLoop, int *scan)
{
   int i;
   for (i=0; i<numLoop; ++i) {
      if (scan >= loopRec[i*2] && scan < loopRec[i*2+1])
         break;
   }
   return (i != numLoop);
}

int *lastLoc[REN_BUF];
int *firstLoc[REN_BUF];

static int rename_register2(int *instInfo, int *funcBegin, int *funcEnd,
                            int base_, int dofloat, int tmask)
{
   int *scan;
   int i, j, numReg = 0;
   int memMask, altMask;
   int memInst, altInst;
   int lb = 1 << 20; // load bit
   int base = base_;
   int maxReg = popcount32b(base);
   int reg[32];
   int hash[REN_BUF];
   int offset[REN_BUF];
   int count[REN_BUF];
   int loopCount[REN_BUF];
   int lastStr[REN_BUF];

   if (maxReg == 0) return base;

   int **loopRec = (int **) malloc((funcEnd-funcBegin)*sizeof(int *)/2);
   int numLoop = find_loop(loopRec, funcBegin, funcEnd);

   if (dofloat) {
      memMask = 0xff200f00;
      altMask = 0x0f200000;
      memInst = 0xed000a00;
      altInst = 0x05000000;
   }
   else {
      memMask = 0x0f200000;
      altMask = 0xff200f00;
      memInst = 0x05000000;
      altInst = 0xed000a00;
   }

   int rmask = tmask;

   if (rmask != (1 << rFP)) {
      int emask = memMask & 0xfedfffff;
      int einst = memInst & 0xfedfffff;

      // exclude registers that use pre/post increment
      for (scan = funcBegin; scan <= funcEnd; ++scan) {
         scan = skip_nop(scan, S_FWD);

         if ((*scan & emask) == einst &&
             (*scan & 0x01200000) != 0x01000000) {
            rmask &= ~(1 << ((*scan & RI_Rn) >> 16));
         }
      }

      if (dofloat) memMask |= 0x0000f000; // only xform s0/s1 loads/stores
   }

   if (rmask == 0) return base;

   for (i=0; i<REN_BUF; ++i) {
      count[i] = 0;
      loopCount[i] = 0;
      lastStr[i] = 0;
      firstLoc[i] = (int *) 0;
   }

   /* record frame variable in this context */
   for (scan = funcBegin; scan <= funcEnd; ++scan) {
      scan = skip_nop(scan, S_FWD);

      if ((*scan & memMask) == memInst &&
          ((1 << ((*scan & RI_Rn) >> 16)) & rmask)) {
         int off = dofloat ? ((*scan & 0xff)*4) : (*scan & 0xfff);
         if ((*scan & (1<<23)) == 0) off = -off;
         int h = (*scan & RI_Rn) | (off & ~RI_Rn);
         for (i = 0; i < numReg; ++i) {
            if (hash[i] == h) break;
         }
         if (i == numReg) {
            if (numReg == REN_BUF) break;
            hash[numReg] = h;
            offset[numReg++] = off;
         }
         ++count[i];
         if (in_loop(loopRec, numLoop, scan)) ++loopCount[i];
         if (extra_opt) count[i] |= ((dofloat ? 2 : 1) << 29);
      }
      else if (extra_opt && (*scan & altMask) == altInst &&
               ((1 << ((*scan & RI_Rn) >> 16)) & rmask)) {
         int off = dofloat ? (*scan & 0xfff) : ((*scan & 0xff)*4);
         if ((*scan & (1<<23)) == 0) off = -off;
         int h = (*scan & RI_Rn) | (off & ~RI_Rn);
         for (i = 0; i < numReg; ++i) {
            if (hash[i] == h) break;
         }
         if (i == numReg) {
            if (numReg == REN_BUF) break;
            hash[numReg] = h;
            offset[numReg++] = off;
         }
         // cross-access disqualifies xform
         count[i] |= ((dofloat ? 1 : 2) << 29);
      }
   }

   if (extra_opt) {
      for (i=0; i<numReg; ++i) {
         // This test is 'too conservative' and can discard block scope
         // loc-var declarations where 'sibling' scopes have an int
         // declared in one scope overlap a float declared in the other.

         if ((count[i] & (3 << 29)) != ((dofloat ? 2 : 1) << 29)) {
            --numReg;
            for(j = i; j < numReg; ++j) {
               hash[j] = hash[j+1];
               offset[j] = offset[j+1];
               count[j] = count[j+1];
               loopCount[j] = loopCount[j+1];
            }
            --i;
         }
         else
            count[i] &= 0x0fffffff;
      }
   }

   /* discard any frame variables that are not trivial */
   for (scan = funcBegin; scan <= funcEnd; ++scan) {
      scan = skip_nop(scan, S_FWD);

      // this comparison is a hack -- not rigorous
      if (((*scan & 0xfff00000) == 0xe2800000 ||  // add r0, fp, #X
           (*scan & 0xfff00000) == 0xe2400000) && // sub r0, fp, #X
           (rmask & (1 << ((*scan & RI_Rn) >> 16))) ) {
         int off = arith_off(*scan);
         if ((*scan & 0xfff00f00) == 0xe2400000) {
            off = -off;
         }
         if ((*scan & RI_Rn) == 0x000b0000) {
            int h = (*scan & RI_Rn) | (off & ~RI_Rn);
            for (i = 0; i < numReg; ++i) {
               if (hash[i] == h) break;
            }
            if (i != numReg) {
               --numReg;
               for(; i < numReg; ++i) {
                  hash[i] = hash[i+1];
                  offset[i] = offset[i+1];
                  count[i] = count[i+1];
                  loopCount[i] = loopCount[i+1];
               }
            }
         }
         else {
            int h = (*scan & RI_Rn);
            for (i = 0; i < numReg; ++i) {
               if ((hash[i] & RI_Rn) == h) {
                 --numReg;
                  for(j = i; j < numReg; ++j) {
                     hash[j] = hash[j+1];
                     offset[j] = offset[j+1];
                     count[j] = count[j+1];
                     loopCount[j] = loopCount[j+1];
                  }
                  --i;
               }
            }
         }
      }
   }

   int done;
   do {
      done = 1;
      for (i=0; i<numReg-1; ++i) {
         if (loopCount[i]  *256 + count[i] <
             loopCount[i+1]*256 + count[i+1]) {
            int tmp = count[i];
            count[i] = count[i+1];
            count[i+1] = tmp;
            tmp = loopCount[i];
            loopCount[i] = loopCount[i+1];
            loopCount[i+1] = tmp;
            tmp = offset[i];
            offset[i] = offset[i+1];
            offset[i+1] = tmp;
            tmp = hash[i];
            hash[i] = hash[i+1];
            hash[i+1] = tmp;
            done = 0;
         }
      }
   } while(!done);

   // do not waste register on low count operation
   for (i=0; i<numReg; ++i)
      if (loopCount[i]*256 + count[i] < 2) break;

   if (!dofloat) {

      // put passed parameters in registers
      do {
         done = 1;
         for (j=i; j<numReg-1; ++j) {
            if (offset[j] < offset[j+1]) {
               int tmp = count[j];
               count[j] = count[j+1];
               count[j+1] = tmp;
               tmp = loopCount[j];
               loopCount[j] = loopCount[j+1];
               loopCount[j+1] = tmp;
               tmp = offset[j];
               offset[j] = offset[j+1];
               offset[j+1] = tmp;
               done = 0;
               tmp = hash[j];
               hash[j] = hash[j+1];
               hash[j+1] = tmp;
            }
         }
      } while(!done);

      int k;
      for (k=i; k<numReg; ++k)
         if (offset[j] < 0) break;

      do {
         done = 1;
         for (j=i; j<k-1; ++j) {
            if (loopCount[j]  *256 + count[j] <
                loopCount[j+1]*256 + count[j+1]) {
               int tmp = count[j];
               count[j] = count[j+1];
               count[j+1] = tmp;
               tmp = loopCount[j];
               loopCount[j] = loopCount[j+1];
               loopCount[j+1] = tmp;
               tmp = offset[j];
               offset[j] = offset[j+1];
               offset[j+1] = tmp;
               tmp = hash[j];
               hash[j] = hash[j+1];
               hash[j+1] = tmp;
               done = 0;
            }
         }
      } while(!done);

      for (j=i; j<k; ++j)
         if (loopCount[j]*256 + count[j] < 2) break;

      numReg = j;
   }
   else {
      numReg = i;

      for (i=0; i<numReg; ++i) offset[i] >>= 2;
   }

   if (numReg == 0) { free(loopRec); return base; }

   if (numReg > maxReg) numReg = maxReg;

   if (dofloat) // dependency info
      create_inst_info_f(instInfo, funcBegin, funcEnd);
   else
      create_inst_info(instInfo, funcBegin, funcEnd);

   create_bb_info(instInfo, funcBegin, funcEnd);

   for (i=0; i<numReg; ++i) {
      base ^= (1 << (reg[i] = avail_reg(base)));
   }

   /* create registers for frame vars */
   for (scan = funcBegin; scan <= funcEnd; ++scan) {
      scan = skip_nop(scan, S_FWD);

      if ((*scan & memMask) == memInst &&
          ((1 << ((*scan & RI_Rn) >> 16)) & rmask)) {
         int rd;
         int *rdu, *rdd, *rdt, *rfinal;
         int off = dofloat ? ((*scan & 0xff)*4) : (*scan & 0xfff);
         if ((*scan & (1<<23)) == 0) off = -off;
         int h = (*scan & RI_Rn) | (off & ~RI_Rn);

         for (i = 0; i < numReg; ++i) {
            if (hash[i] == h) break;
         }
         if (i == numReg) continue; // this frame var not mapped

         rd = dofloat ? (((*scan & RI_Rd) >> 11) + ((*scan & RI_Sd) >> 22)) :
                        ((*scan & RI_Rd) >> 12);

         if ((*scan & lb)) { // load [rn, #X]
            rdd = find_def(instInfo, &instInfo[(scan-funcBegin)+1], rd, S_FWD);
            rdu = find_use(instInfo, &instInfo[(scan-funcBegin)+1], rd, S_FWD);
            if (rdu != 0) {
               if (rdd == 0) rdd = &instInfo[funcEnd-funcBegin];
               rfinal = find_use_precede_def(instInfo, rdu, rdd, rd, S_FWD);
               for (rdt = &instInfo[(scan-funcBegin)+1];
                    rdt <= rfinal; ++rdt) if (*rdt & RI_bb) break;
            }
            if (firstLoc[i] == 0) firstLoc[i] = scan;
            if (rdu == 0 || rdt <= rfinal || // rdu == 0 means func ret value
                funcBegin[rdu-instInfo] == 0xe3500000) { // switch stmt
               if (dofloat) {
                  *scan = 0xeeb00a40 | (*scan & 0x0040f000) |
                         (reg[i] >> 1) | ((reg[i] & 1) << 5);
                  instInfo[scan-funcBegin] = RI_RdDest | RI_RdAct | RI_RmAct |
                     (reg[i] & 0x0f) | ((reg[i] & 0x10) << 16) |
                     ((*scan & RI_Sd) >> 10) | ((*scan & 0x7000) * 2) |
                     ((*scan & 0x8000) << 7) |
                     (instInfo[scan-funcBegin] & RI_bb);
               }
               else {
                  *scan = 0xe1a00000 | (*scan & RI_Rd) | reg[i];
                  instInfo[scan-funcBegin] = RI_RdDest | RI_RdAct |
                     RI_RmAct | (*scan & RI_Rd) | reg[i] |
                     (instInfo[scan-funcBegin] & RI_bb);
               }
            }
            else {
               *scan = NOP;
               instInfo[scan-funcBegin] &= RI_bb;

               do {
                  int *rscan = &funcBegin[rdu-instInfo];
                  if ((*rscan & memMask) == memInst && (*rscan & lb) == 0 &&
                      ((1 << ((*rscan & RI_Rn) >> 16)) & rmask)) {
                     // frame var store
                     lastStr[i] = *scan;
                     lastLoc[i] = scan;
                     int off2 = dofloat ? ((*rscan & 0xff)*4) : (*rscan & 0xfff);
                     if ((*rscan & (1<<23)) == 0) off2 = -off2;
                     int hh = (*rscan & RI_Rn) | (off2 & ~RI_Rn);
                     for (j = 0; j < numReg; ++j) {
                        if (hash[j] == hh) break;
                     }
                     if (j != numReg) {
                        if (i == j) {
                           *rscan = NOP;
                           instInfo[rscan-funcBegin] &= RI_bb;
                        }
                        else {
                           if (dofloat) {
                              *rscan = 0xeeb00a40 | ((reg[j] & 0x1e) << 11) |
                                       ((reg[j] & 1) << 22) |
                                       (reg[i] >> 1) | ((reg[i] & 1) << 5);
                              instInfo[rscan-funcBegin] =
                                 RI_RdDest | RI_RdAct | RI_RmAct |
                                 ((reg[j] & 0x0f) << 12) |
                                 ((reg[j] & 0x10) << 18) |
                                 (reg[i] & 0x0f) | ((reg[i] & 0x10) << 16) |
                                 (instInfo[rscan-funcBegin] & RI_bb);
                           }
                           else {
                              *rscan = 0xe1a00000 | (reg[j] << 12) | reg[i];
                              instInfo[rscan-funcBegin] =
                                 RI_RdDest | RI_RdAct | RI_RmAct |
                                 (reg[j] << 12) | reg[i] |
                                 (instInfo[rscan-funcBegin] & RI_bb);
                           }
                        }
                        goto nextUse;
                     }
                  }
                  if (dofloat)
                     reg_rename_f(reg[i], rd, rdu, rscan);
                  else
                     reg_rename(reg[i], rd, rdu, rscan);
nextUse:
                  if (rdu == rfinal) break;
                  rdu = find_use(instInfo, &instInfo[(rscan-funcBegin)+1],
                                 rd, S_FWD);
               } while (1);
            }
         }
         else { // store [rn, #X]
            lastStr[i] = *scan;
            lastLoc[i] = scan;
            if (dofloat) {
               *scan = 0xeeb00a40 |
                       ((reg[i] & 0x1e) << 11) | ((reg[i] & 1) << 22) |
                       ((*scan & RI_Rd) >> 12) | ((*scan & RI_Sd) >> 17);
               instInfo[scan-funcBegin] = RI_RdDest | RI_RdAct | RI_RmAct |
                  ((reg[i] & 0x0f) << 12) | ((reg[i] & 0x10) << 18) |
                  ((instInfo[scan-funcBegin] & RI_Rd) >> 12) |
                  ((instInfo[scan-funcBegin] & RI_Sd) >> 2) |
                  (instInfo[scan-funcBegin] & RI_bb);
            }
            else {
               *scan = 0xe1a00000 | (reg[i] << 12) | ((*scan & RI_Rd) >> 12);
               instInfo[scan-funcBegin] = RI_RdDest | RI_RdAct | RI_RmAct |
                  (reg[i] << 12) | ((*scan & RI_Rd) >> 12) |
                  (instInfo[scan-funcBegin] & RI_bb);
            }
         }
      }
   }

   // hack to handle "last" store instruction.  Only works if
   // literal last store is in a non-conditional code path
   for (i = 0; i < numReg; ++i) {  // ldr rd, [rd, #X]
      if (lastStr[i] && ((hash[i] & RI_Rn) != 0x000b0000)) {
         if (dofloat) {
            *lastLoc[i] = lastStr[i];
            instInfo[lastLoc[i]-funcBegin] = RI_RdAct |
                  ((lastStr[i] & 0x7000) * 2) |
                  ((lastStr[i] & RI_Sd) >> 10) |
                  ((lastStr[i] & 0x8000) << 7) |
                  (instInfo[lastLoc[i]-funcBegin] & RI_bb);
         }
         else {
            *lastLoc[i] = lastStr[i];
            instInfo[lastLoc[i]-funcBegin] = RI_RdDest | RI_RdAct |
               (lastStr[i] & RI_Rd) |
               (instInfo[lastLoc[i]-funcBegin] & RI_bb);
         }
      }
   }

   /* load frame vars into registers at top of function */
   for (scan = funcBegin; *scan != NOP; ++scan);
   j = 0;
   for (i = 0; i < numReg; ++i) {  // ldr rd, [rn, #X]
      if (offset[i] >= 0) {
         if ((hash[i] & RI_Rn) == 0x000b0000) { // frame ptr
            if (*scan != NOP) {
               printf("out of register2 assignment space\n");
               exit(-1);
            }
            if (dofloat)
               *scan++ = 0xed900a00 | (hash[i] & RI_Rn) |
                         ((reg[i] & 0x1e) << 11) | ((reg[i] & 1) << 22) |
                         offset[i];
            else
               *scan++ = 0xe5900000 | (hash[i] & RI_Rn) |
                         (reg[i] << 12) | offset[i];
         }
         else if (firstLoc[i] != 0) {
            int *fl = firstLoc[i];
            if (dofloat)
               *fl = 0xed900a00 | (hash[i] & RI_Rn) |
                     ((reg[i] & 0x1e) << 11) | ((reg[i] & 1) << 22) |
                     offset[i];
            else
               *fl = 0xe5900000 | (hash[i] & RI_Rn) |
                     (reg[i] << 12) | offset[i];
         }
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

   free(loopRec);

   return base;
}

/**********************************************************/
/********* Peephole optimization driver function **********/
/**********************************************************/

int *squint_opt(int *begin, int *end) // "end" points past last inst
{
   int *lastInstruction = end ;
   int *scan = begin;
   int *tmpbuf = (int *) malloc((end-begin+2)*sizeof(int));
   int noFloatConst;

   create_const_map(begin, end);
   const_imm_opt(begin, end);

   int iregMask = 0x000057f8; // r3-10, r12(ip), r14(lr) available
   int fregMask = 0xfffffffc; // 32 FP registers available

   while (scan < end) {
      if (*scan == 0xe92d4800 && !is_const(scan)) { // push {fp, lr}
         int *funcBegin = scan; // first instr of new func
         int *funcEnd;
         int *retAddr = 0;
         ++scan;
         while (scan < end) {
            if (*scan == 0xe92d4800 && !is_const(scan)) { // push {fp, lr}
               break;
            }
            else if (*scan == 0xe8bd8800 && !is_const(scan)) { // pop {fp, pc}
               retAddr = scan; // last active inst in func
            }
            ++scan;
         }
         --scan;
         funcEnd = scan; // inst before EOF or before next func

         // verify this function has been prepared for peephole opt
         if (funcBegin[3] != NOP) continue;

         int ilow, flow, fhigh;
         int hasFuncCall = 0;

         // check for function calls
         for (scan = funcBegin; scan <= funcEnd; ++scan) {
            if (*scan == NOP11 && !is_const(scan)) {
               hasFuncCall = 1;
               break;
            }
         }
         scan = funcEnd;

         /******************************************/
         /***   convert stack VM to frame VM     ***/
         /******************************************/

         int ireg = iregMask;
         int freg = fregMask;

         // retAddr points to last ret in function
         skip_const_blk = 0;
         simplify_branch(funcBegin, retAddr);
         skip_const_blk = 1;

         apply_peepholes1(funcBegin, retAddr);
         apply_peepholes2(tmpbuf, funcBegin, retAddr);
         apply_peepholes1(funcBegin, retAddr);
         apply_peepholes3(tmpbuf, funcBegin, retAddr);
         apply_peepholes3_2(funcBegin, retAddr);

         create_pushpop_map(tmpbuf, funcBegin, retAddr);

         if (!hasFuncCall)
            freg = rename_register1(tmpbuf, funcBegin, retAddr, freg);

         skip_const_blk = 0;
         apply_peepholes3_5(funcBegin, retAddr);
         skip_const_blk = 1;
         apply_peepholes1(funcBegin, retAddr);

         noFloatConst = *(funcBegin - 1) == NOP;
         create_pushpop_map2(tmpbuf, funcBegin, retAddr);
         apply_peepholes4(tmpbuf, funcBegin, retAddr);
         apply_peepholes4_2(tmpbuf, funcBegin, retAddr);

         /******************************************/
         /***  convert frame VM to register VM   ***/
         /******************************************/

         ilow = avail_reg(ireg);
         if (!hasFuncCall)
            ireg = rename_register2(tmpbuf, funcBegin, retAddr,
                                    ireg, 0, (1 << rFP));

         if (!noFloatConst) // correction for rename_register1
            apply_peepholes4_5(tmpbuf, funcBegin, retAddr);

         flow = avail_reg(freg);
         freg = create_pushpop_map3(tmpbuf, funcBegin, retAddr,
                                    hasFuncCall, freg, 1);
         fhigh = avail_reg(freg);

         apply_peepholes4_6(tmpbuf, funcBegin, retAddr);
         if (!hasFuncCall) {
            int tmp = (freg & 0xf0000000);
            freg = tmp | rename_register2(tmpbuf, funcBegin, retAddr,
                                          (freg & 0x0fffffff), 1, (1 << rFP));
         }

         apply_peepholes4_7(tmpbuf, funcBegin, retAddr);
         // apply_peepholes5(funcBegin, retAddr);
         apply_peepholes6(tmpbuf, funcBegin, retAddr, ilow,
                          avail_reg(ireg), 0);

         if (!noFloatConst)
            apply_peepholes6(tmpbuf, funcBegin, retAddr, flow, fhigh, 1);

         create_pushpop_map2b(tmpbuf, funcBegin, funcEnd);
         ireg = create_pushpop_map3(tmpbuf, funcBegin, retAddr,
                                    hasFuncCall, ireg, 0);
         apply_peepholes7(tmpbuf, funcBegin, retAddr);
         create_pushpop_map4(tmpbuf, funcBegin, funcEnd, 0);
         create_pushpop_map4(tmpbuf, funcBegin, funcEnd, 1);

         apply_peepholes7_5(tmpbuf, funcBegin, retAddr);
         apply_peepholes7_6(funcBegin, retAddr);
         apply_peepholes7_7(funcBegin, retAddr);

         if (extra_opt)
            freg = apply_ptr_cleanup(tmpbuf, funcBegin, retAddr, freg);
         apply_peepholes7_8(tmpbuf, funcBegin, retAddr);

         rename_nop(funcBegin, retAddr);

         apply_peepholes8(tmpbuf, funcBegin, retAddr, flow, fhigh);
         apply_peepholes8_1(tmpbuf, funcBegin, retAddr);
         apply_peepholes8_2(tmpbuf, funcBegin, funcEnd, &freg);
         apply_peepholes9(tmpbuf, funcBegin, retAddr, ireg);
         if (!hasFuncCall)
            freg = rename_register2(tmpbuf, funcBegin, retAddr, freg, 1,
                                    (UREG_MASK & ~((1 << rFP) | 7)));

         // if (noFloatConst)
         //    simplify_frame(funcBegin, retAddr); Also,
         //    re-enable guard at top of rename_register1()

         // relocate_nop *must* be the last optimization on a
         // function due to 'destructive' remapping of const_data
         lastInstruction = relocate_nop(funcBegin, funcEnd, IntraFunc);
      }
      else {
         ++scan;
      }
   }
#ifndef NO_PACK_ICACHE
   lastInstruction = relocate_nop(begin, end - 1, InterFunc);
#endif
   destroy_const_map();
   free(tmpbuf);
   return lastInstruction;
}

#ifndef SQUINT_SO

/**********************************************************/
/******** read exe, optimize, write exe *******************/
/**********************************************************/

#ifdef __MC__
#define O_RDWR 2
#define SEEK_SET 0
typedef int off_t;
typedef int size_t;
#endif

int main(int argc, char *argv[])
{
   int fd, retVal = 0;
   int offset, length;
   int *mem;

   if (argc == 5 && argv[4][0] == '-' && argv[4][1] == 'e')
      extra_opt = 0;
   else if (argc != 4) {
      printf("Use: %s <mc executable> <text start> <length>\n", argv[0]);
      exit(-1);
   }

   offset = strtoul(argv[2], 0, 16);
   length = strtoul(argv[3], 0, 16);

   if (!(offset > 0 && length > 0)) {
       printf("last two arguments must be hex file offset and length\n");
       exit(-1);
   }

   mem = (int *) malloc(length);

   if ((fd = open(argv[1], O_RDWR)) < 0) {
      printf("could not open file %s\n", argv[1]);
      retVal = -2;
   }

   else if (lseek(fd, (off_t) offset, SEEK_SET) != offset) {
      printf("could not seek to offset %x in file %s.\n", offset, argv[1]);
      retVal = -1;
   }

   else if (read(fd, mem, (size_t)length) != length) {
      printf("could not read %x bytes from file %s.\n", length, argv[1]);
      retVal = -1;
   }

   else if (!squint_opt(mem, mem + length/4)) {
      printf("Compile with mc -Op flag to enable peephole optimizer\n");
   }

   else if (lseek(fd, (off_t) offset, SEEK_SET) != offset) {
      printf("could not seek to offset %x in file %s.\n", offset, argv[1]);
      retVal = -1;
   }

   else if (write(fd, mem, (size_t)length) != length) {
      printf("error occured attempting to write file %s\n", argv[1]);
      retVal = -1;
   }

   if (retVal != -2) close(fd);
   free(mem);

   printf("executable file %s was optimized.\n", argv[1]);

   return retVal;
}

#endif
