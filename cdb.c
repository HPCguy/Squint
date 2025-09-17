// debugger commands:
//
// 1 = go to first line of window, z = go to last line of window
// j = up one line, k = down one line, u = up a page, d = down a page
// x = center disassemly on curr bkpt (toggle)
// l = toggle scroll mode when not centered on bkpt (cursor float or fixed)
// f = toggle focus between assembly or stack frame (0 = center on fp+0)
//
// b = set/clear breakpoint at cursor location
// n = step through current instruction
// s = step into current instruction (like n but enters user subroutines)
// c = continue running to next breakpoint or program exit
//
// q (or ctl-c) = quit

#include <fcntl.h>
#include <unistd.h>
#include <stdio.h>
#include <string.h>
#include <stdlib.h>
#include <signal.h>
#include <termios.h>
#include <sys/ioctl.h>
#include <sys/ptrace.h>
#include <sys/wait.h>
#include <sys/user.h>

#define LINE_BLOCK 4096
#define BKPT_INST  0xe1200070

//   =============================================
//    SYSTEM HEADER INFORMATION
//   =============================================

#ifdef __MC__
typedef int FILE;
#define STDIN_FILENO  0
#define STDOUT_FILENO 1

#define O_RDONLY 0

#define TIOCGWINSZ 21523

typedef char   cc_t;
typedef int speed_t;
typedef int tcflag_t;

#define NCCS 32
struct termios {
    tcflag_t c_iflag;      /* input mode flags */
    tcflag_t c_oflag;      /* output mode flags */
    tcflag_t c_cflag;      /* control mode flags */
    tcflag_t c_lflag;      /* local mode flags */
    cc_t c_line;           /* line discipline */
    cc_t c_cc[NCCS];       /* control characters */
    speed_t c_ispeed;      /* input speed */
    speed_t c_ospeed;      /* output speed */
    int pad;
};
#define TCSANOW  0
#define ICANON   2
#define ECHO     8
#define VTIME    5
#define VMIN     6

typedef int pid_t;

#define PTRACE_TRACEME     0
#define PTRACE_PEEKTEXT    1
#define PTRACE_PEEKDATA    2
#define PTRACE_POKETEXT    4
#define PTRACE_CONT        7
#define PTRACE_GETREGS    12
#define PTRACE_GETVFPREGS 27

#define SIGTRAP   5
#define SIGWINCH 28

typedef int ireg_t;

struct user_regs
{
  ireg_t uregs[18];
};
#endif

#ifndef __MC__
typedef unsigned long ireg_t;
#endif

//   =========================================================
//    GLOBAL VARIABLES
//   =========================================================

int cns_width, cns_height, cns_height_old, cns_width_old, show_fp;
int win_flags = 33; // bit 0 = ireg, 1 = vfp regs, 2-3 = output (tri-state)
int redraw = 1;
int insts = 0;
int resurrect_bp = -1;

#define MAX_BP 120
struct {
   ireg_t addr; // address of bkpt
   int inst;    // instruction bkpt hides
   int flags;   // 4 = HARD FAULT, 2 = cdb created, 1 = user created
} bp_info[MAX_BP];
int bp_free_head = 0;

enum { ASM_USR, ASM_BP, FP, IREG, FREG, CNS_OUT, NUM_WIN };
enum { BP_USR = 1, BP_SDB = 2, BP_SYS = 4 } ;

struct win {
   char cns_y;   // offset for console upper left
   char height;  // height of this window
   char width;   // e.g. fp win is width 16
   char yoff;    // (was sy) focus record, 0..height-1
   int uly;      // first record
   int lly;      // last record ( = uly+(height-1), or max records)
} w[NUM_WIN];

//   ========================================================
//    WINDOW RESIZE HANDLER
//   ========================================================

#ifdef __MC__
int *func_addr(int *pc)
{
  int *i = (pc - 3);
  while (*i != 0xe92d4800) --i;
  return i;
}
#endif

void resize_asm_win(struct win *wn)
{
   wn->height = cns_height;
   wn->width = cns_width;
   wn->lly = wn->uly + (wn->height-1);
   if (wn->lly >= insts) wn->lly = insts-1;
   int yoff_cap = (wn->lly - wn->uly);
   if (wn->yoff > yoff_cap) wn->yoff = yoff_cap;
}

void resize_fp_win(struct win *fp_win)
{
   int asm_height = w[ASM_USR].lly - w[ASM_USR].uly + 1;
   fp_win->cns_y = 0;
   fp_win->height = cns_height;
   fp_win->width = cns_width;
   fp_win->yoff = asm_height/2;
   fp_win->uly = -(asm_height/2);
   fp_win->lly = w[FP].uly + asm_height-1;
}

int *winch_addr;

void winch_handler(int none)
{
   if (winch_addr == 0)
#ifdef __MC__
      winch_addr = func_addr((int *)ARMreg(15));
#endif
#ifndef __MC__
      winch_addr = (int *)winch_handler;
#endif
   else {
      int winfob[4];
      if (ioctl(STDIN_FILENO, TIOCGWINSZ, winfob) == 0)
      {
         cns_height = (winfob[0]         & 0xffff) - (win_flags >> 4);
         cns_width  = ((winfob[0] >> 16) & 0xffff);

         if (cns_height != cns_height_old ||
             cns_width != cns_width_old) { // remap all windows

            resize_asm_win(&w[ASM_USR]);
            resize_asm_win(&w[ASM_BP]);
            resize_fp_win(&w[FP]);

            cns_height_old = cns_height;
            cns_width_old = cns_width;
            redraw = 1;
         }
      }
   }
}

// handle dynamic window sizing
int winsize_init()
{
   int sa[36];
   int winfo[4];

   if (ioctl(STDIN_FILENO, TIOCGWINSZ, winfo) == 0)
   {
      cns_height = (winfo[0]         & 0xffff) - (win_flags >> 4);
      cns_width  = ((winfo[0] >> 16) & 0xffff);
      cns_height_old = cns_height;
      cns_width_old = cns_width;
   }
   else {
      return 1;
   }

   // install window resize signal handler
   memset(sa, 0, sizeof(sa));
   winch_handler(0);
   sa[0] = (int) winch_addr;
#ifdef __MC__
   sigaction(SIGWINCH, sa, 0);
#endif
#ifndef __MC__
   sigaction(SIGWINCH, (struct sigaction *)sa, 0);
#endif

   return 0;
}

//   ========================================================
//    'REGISTER WINDOW' DISPLAY
//   ========================================================

void show_regs(int *r, char *oline)
{
   char buffer[16];
   strcpy(oline, "\n");
   for (int i=0; i<17; ++i) {
      sprintf(buffer, "%08x ", r[i]);
      strcat(oline, buffer);
      if (i == 7) strcat(oline, "\n");
   }
   int len = strlen(oline);
   oline[--len] = 0;
   write(STDOUT_FILENO, oline, len);
}

//   ========================================================
//    Capture disassembly for executable text segment
//   ========================================================

// Execute "objdump -d" on executable to disassemble ELF .text segment
// and store each disassembled line in text argument. n is # of lines
int get_disassembly(char *executable_name, char ***text, int *n)
{
   char shell_str[128];
   char iline[128], oline[128];
   int max_lines = LINE_BLOCK;
   char **ltext = (char **)malloc(max_lines*sizeof(char *));
   int max_asm_width = 0;
   int nn = 0;

   sprintf(shell_str, "scripts/disasm %s", executable_name);
   FILE *asm_fp = (FILE *) popen(shell_str, "r");
   while(!feof(asm_fp)) {
      int i=0, j=0;
      if (!fgets(iline, 128, asm_fp)) break;
      if (nn == max_lines) {
         max_lines += LINE_BLOCK;
         ltext = (char **)realloc(ltext, max_lines*sizeof(char *));
      }
      // expand tabs
      while (iline[i] != 0) {
         if (iline[i] == '\t') {
            if ((j & 3) == 0) oline[j++] = ' ';
            while (j & 3) oline[j++] = ' ';
            ++i;
         }
         else
            oline[j++] = iline[i++];
      }
      if (oline[j-1] == '\n') --j;
      if (j > max_asm_width) max_asm_width = j;
      oline[j++] = 0;
      memcpy(ltext[nn++] = (char *) malloc(j), oline, j);
   }
   pclose(asm_fp);
   *text = ltext;
   *n = nn;
   return max_asm_width;
}

//   ========================================================
//    Grab a slice of memory from traced executable
//   ========================================================

// len is number of integers to grab
void get_frame_slice(pid_t child, int *fp, int low_addr, int len)
{
   int addr = low_addr;
   for (int i=0; i<len; ++i) {
      fp[i] = ptrace(PTRACE_PEEKDATA, child, addr, 0);
      addr += 4;
   }
}

//   ========================================================
//    Breakpoint support code
//   ========================================================

// center window on current instruction
void recenter_inst(struct win *wn, int ci)
{
   wn->cns_y = 0;
   wn->height = cns_height;
   wn->width = cns_width;

   if (insts <= wn->height) {
      wn->yoff = ci;   // focus line (highlighted)
      wn->uly = 0;
      wn->lly = insts-1;
   }
   else if (insts - ci <= wn->height/2) {
      wn->lly = insts - 1;    // use '<=' in for loops for bound test
      wn->uly = insts - wn->height;
      wn->yoff = ci - wn->uly;
   }
   else {
      wn->yoff = (ci < wn->height/2) ? ci : (wn->height/2); // focus line
      wn->uly = ci - wn->yoff;
      wn->lly = wn->uly + wn->height-1;
   }
}

// Currently, not a "real" toggle.
int bp_toggle(pid_t child, char *bkpt_idx, ireg_t addr, int inst,
              int bp_type, int admin) 
{
   int bp_idx;
   if (resurrect_bp != -1) {
      if ((bp_info[resurrect_bp].flags & BP_SYS) == 0 &&
          ptrace(PTRACE_POKETEXT, child,
                 bp_info[resurrect_bp].addr, BKPT_INST) < 0) {
         perror("PTRACE_POKETEXT set breakpoint error");
         return 1;
      }
      resurrect_bp = -1;
   }
   if (*bkpt_idx == 0) {
      *bkpt_idx = bp_free_head + 1;
      bp_idx = *bkpt_idx - 1;
      bp_free_head = bp_info[bp_idx].flags;
      bp_info[bp_idx].addr = addr;
      bp_info[bp_idx].inst = inst;
      // *** CORRECTNESS RELIES ON if-condition short-circuit exit
      if (admin == 0 && (bp_type & BP_SYS) == 0 &&
          ptrace(PTRACE_POKETEXT, child,
                 bp_info[bp_idx].addr, BKPT_INST) < 0) {
         perror("PTRACE_POKETEXT set breakpoint error");
         return 1;
      }
      bp_info[bp_idx].flags = bp_type + ((bp_type & BP_SDB) ? 8 : 0);
   }
   else { // guaranteed admin == 0 here
      bp_idx = *bkpt_idx - 1;
      int bpf = bp_info[bp_idx].flags ;
      bpf = (bpf & ~(bp_type & ~BP_SYS)) - ((bpf & BP_SDB) ? 8 : 0);
      if ((bpf & 7) == 0) { // close out bp
         if (ptrace(PTRACE_POKETEXT, child,
                    bp_info[bp_idx].addr, bp_info[bp_idx].inst) < 0) {
            perror("PTRACE_POKETEXT restore inst at bkpt");
            return 1;
         }
         if (bpf & 0xf8) { // return from func-call not depleted yet
            bp_info[bp_idx].flags =bpf | BP_SDB;
            resurrect_bp = bp_idx;
         }
         else {
            bp_info[bp_idx].addr = 0; // mark as unused
            bp_info[bp_idx].flags = bp_free_head;
            bp_free_head = bp_idx;
            *bkpt_idx = 0;
         }
      }
      else
         bp_info[bp_idx].flags = bpf;
   }
   return 0;
}

//   ========================================================
//    This is the actual ptrace debugger
//   ========================================================

void debug(pid_t child, char *trace_executable_name, ireg_t text_base_ptr)
{
   int fp[96]; // stack window
   char oline[128];
   struct termios tty, tty2;
   char hl[64]; // highlights
   char format[32];
   char **text = 0; // disassmebly lines
   int max_asm_width;
   struct user_regs iregs;
   int wait_status;
   ireg_t curr_FP, curr_SP;
   int ub, lb;
   int ll = 1; // line lock: 0 = move cursor, 1 = scroll

   // Run until bkpt instruction encountered
   waitpid(child, &wait_status, 0);
   if (ptrace(PTRACE_CONT, child, 0, 0) < 0) {
      perror("PTRACE_CONT failure");
      return;
   }
   waitpid(child, &wait_status, 0);

   int estat = (wait_status >> 8) & 0xff;
   if ((wait_status & 0x7f) == 0) { // Normal program exit
      printf("program %s ran to completion without stopping, status = %d\n",
         trace_executable_name, estat | ((estat & 0x80) ? 0xffffff00 : 0));
      return;
   }

   max_asm_width = get_disassembly(trace_executable_name, &text, &insts);

   // set up free list for breakpoint records
   char *bkpt = (char *) calloc(1, insts);
   for (int i=0; i<MAX_BP-1; ++i)
      bp_info[i].flags = i+1;
   bp_info[MAX_BP-1].flags = -1;

   ptrace(PTRACE_GETREGS, child, 0, &iregs);
   ireg_t addr = iregs.uregs[15];
   int inst = ptrace(PTRACE_PEEKTEXT, child, addr, 0);
   int ci = (addr - text_base_ptr) / sizeof(inst); // current instruction

   if (estat == SIGTRAP) { // BKPT_INST
      // software breakpoint reached (continuable)
      if (inst != BKPT_INST) {
         printf("Breakpoint was not set in executable.\n");
         return;
      }
      if (strtoul(&text[ci][8], 0, 16) != BKPT_INST) {
         perror("Trace executable not compiled with mc -d option");
         return;
      }
      inst = 0xe92d4800;   // push {fp, lr}
      if (bp_toggle(child, &bkpt[ci], addr, inst, BP_SDB, 1)) return; 
      strcpy(&text[ci][20], "push {fp, lr}");
   }
   else {
      if (bp_toggle(child, &bkpt[ci], addr, inst, BP_SYS, 1)) return;
      ll = 0; // facilitate bug finding
   }

   // manage dynamic width and height of terminal window
   if (winsize_init() != 0) {
      perror("could not get window size\n");
      return;
   }

   // set unbuffered console input for keypress
   tcgetattr(STDIN_FILENO, &tty);
   memcpy(&tty2, &tty, sizeof(struct termios));
   tty2.c_lflag &= ~(ICANON | ECHO);
   tty2.c_cc[VTIME] = 1;  // timeout, 0 = wait forever
   tty2.c_cc[VMIN] = 1;   // Min characters to wait for
   tcsetattr(STDIN_FILENO, TCSANOW, &tty2);

   // center ASM_USR and ASM_BP windows at current bkpt
   recenter_inst(&w[ASM_USR], ci);
   memcpy(&w[ASM_BP], &w[ASM_USR], sizeof(struct win));
   resize_fp_win(&w[FP]);

   curr_FP = iregs.uregs[11]; // push {fp, lr} hasn't happened yet

   struct win *dw = &w[ll ? ASM_BP : ASM_USR]; // disassembly window
   struct win *fw = dw;               // set focus window for cursor
   do {
      curr_SP = iregs.uregs[13];
      if (redraw) {
         int hf[24]; // highlighted FPs
         int nhf = 0;
         int asm_height = w[ASM_USR].lly - w[ASM_USR].uly + 1;
         // stack values automatically shown if window is wide enough
         show_fp = curr_FP != 0 &&
                   ((cns_width - max_asm_width) >= 16 || fw == &w[FP]);

         if (show_fp) {
            get_frame_slice(child, fp, curr_FP-(asm_height+w[FP].uly)*4,
                            asm_height+1);

            // highlight whole chain of frame pointers as long as
            // current FP is included in the visible sliding window
            int scan = (asm_height+w[FP].uly);
            if (scan > 0 && scan < asm_height && fp[scan] != 0) {
               hf[nhf++] = scan;
               scan = (hf[nhf++] = scan + (fp[scan] - curr_FP)/4);
               while(scan < asm_height && fp[scan] != 0 && nhf < 24) {
                  scan = (hf[nhf] = scan + (fp[scan] - fp[hf[nhf-2]])/4);
                  ++nhf;
               }
               if (scan >= asm_height) --nhf;
               --nhf; // set current highlight for later testing
            }
         }

         // clear screen, hide cursor
         sprintf(oline, "\e[H\e[2J\e[?25l");
         write(STDOUT_FILENO, oline, strlen(oline));

         // display the disassembly and/or frame information
         format[0] = '%';
         if (show_fp)
            sprintf(&format[1], "-%d.%ds", (cns_width-16), (cns_width-16));
         else
            sprintf(&format[1], "-.%ds\n", dw->width);
        
         int j = -w[FP].uly*4;
         int k = asm_height;
         int ma = k*4 + curr_FP-(asm_height+w[FP].uly)*4;
         for (int i = dw->uly; i <= dw->lly; ++i) {
            // handle disassembly window
            {
               int bp_flag = bkpt[i] ? bp_info[bkpt[i]-1].flags : 0;
               hl[0] = 0;
               // red should mean impassable fault (aka cont/run impossible)
               if (fw != &w[FP] && i == dw->uly + dw->yoff)
                  strcat(hl, "1;32"); // bright green fore
               if (bp_flag & BP_SYS)
                  strcat(hl, hl[0] ? ";41" : "41"); // red bkg
               else if (bp_flag & BP_SDB)
                  strcat(hl, hl[0] ? ";44" : "44"); // blue bkg
               else if (bp_flag & BP_USR)
                  strcat(hl, hl[0] ? ";46" : "46"); // cyan bkg
               if (hl[0]) {
                  sprintf(oline, "\e[%sm", hl);
                  write(STDOUT_FILENO, oline, strlen(oline));
               }
               if (show_fp == 0 && i == dw->lly)
                  format[strlen(format)-1] = 0;
               sprintf(oline, format, text[i]);
               write(STDOUT_FILENO, oline, strlen(oline));
               if (hl[0]) { // turn off highlights
                  strcpy(oline, "\e[0m");
                  write(STDOUT_FILENO, oline, strlen(oline));
               }
            }
            if (show_fp) { // handle stack frame information
               hl[0] = 0;
               if (fw == &w[FP] && j == -(w[FP].uly + w[FP].yoff)*4)
                  strcat(hl, "1;32"); // bright green fore
               if (ma == curr_SP)
                  strcat(hl, hl[0] ? ";44" : "44"); // blue bkg
               else if (ma == curr_FP)
                  strcat(hl, hl[0] ? ";46" : "46"); // cyan bkg
               else if (nhf > 0 && hf[nhf] == k) { // other frames
                  strcat(hl, hl[0] ? ";43" : "43"); // yellow bkg
                  --nhf;
               }
               if (hl[0]) {
                  sprintf(oline, "\e[%sm", hl);
                  write(STDOUT_FILENO, oline, strlen(oline));
               }
               if (j < 0)
                  sprintf(oline, ((i == dw->lly) ?
                          "|fp-%03x %08x" : "|fp-%03x %08x\n"), -j, fp[k]);
               else if (j > 0)
                  sprintf(oline, ((i == dw->lly) ?
                          "|fp+%03x %08x" : "|fp+%03x %08x\n"), j, fp[k]);
               else
                  sprintf(oline, ((i == dw->lly) ?
                          "|fp     %08x" : "|fp     %08x\n"), fp[k]);
               write(STDOUT_FILENO, oline, strlen(oline));
               if (hl[0]) {
                  strcpy(oline, "\e[0m");
                  write(STDOUT_FILENO, oline, strlen(oline));
               }
               --k;
               ma -= 4;
            }
            j -= 4;
         }

         if (cns_width >= 80) show_regs((int *)&iregs, oline);
         redraw = 0;
      }

      int ch = 0;
      read(STDIN_FILENO, &ch, 1); // Read a single character

      if (ch == 3 || ch == 'q') break; // 3 is ASCII for Ctrl+C

      if (fw == &w[ASM_USR] || fw == &w[ASM_BP]) {
         lb = 0;
         ub = insts;
      }
      else if (fw == &w[FP]) {
         lb = -1023;
         ub =  1024;
      }

      if (ch == 'x') { // toggle: center focus on breakpoint
         dw = ((dw == &w[ASM_USR]) ? &w[ASM_BP] : &w[ASM_USR]);
         if (fw != &w[FP]) fw = dw;
         redraw = 1;
      }
      else if (ch == 'f') { // toggle: focus on disasm or frame vars
         fw = ((fw == dw) ? &w[FP] : dw);
         redraw = 1;
      }
      else if (ch == '0') {
         if (fw == &w[FP]) { // move focus to FP
            resize_fp_win(&w[FP]);
            if (show_fp) redraw = 1;
         }
         else if (fw == &w[ASM_USR]) {
            if (memcmp(&w[ASM_USR], &w[ASM_BP], sizeof(struct win))) {
               memcpy(&w[ASM_USR], &w[ASM_BP], sizeof(struct win));
               redraw = 1;
            }
         }
      }
      else if ((ch == 'n' || ch == 's') &&  // 'next' or 'step' instruction
               (bp_info[bkpt[ci]-1].flags & BP_SYS) == 0) {
         if (inst == 0xe8bd8800) goto bl_ret; // pop {fp, pc}

         int bt = 0; // branch_taken
         if ((inst & 0x0e000000) == 0x0a000000) { // branch inst
            // Cond bits: N = 8, Z = 4, C = 2, V = 1
            int cond = (iregs.uregs[16] >> 28) & 0xf; // condition flags
            switch ((inst >> 28) & 0xf) { // condition code
            case 0x0: if ((cond&4) != 0) bt=1; break; // BEQ
            case 0x1: if ((cond&4) == 0) bt=1; break; // BNE
            case 0x2: if ((cond&2) != 0) bt=1; break; // BHS
            case 0x3: if ((cond&2) == 0) bt=1; break; // BLO
            case 0x4: if ((cond&8) != 0) bt=1; break; // BMI
            case 0x5: if ((cond&8) == 0) bt=1; break; // BPL
            case 0x6: if ((cond&1) != 0) bt=1; break; // BVS
            case 0x7: if ((cond&1) == 0) bt=1; break; // BVC
            case 0x8: if ((cond&6) == 2) bt=1; break; // BHI
            case 0x9: if ((cond&4) == 4 || (cond&2) == 0) bt=1; break; // BLS
            case 0xa: if ((cond&9) == 9 || (cond&9) == 0) bt=1; break; // BGE
            case 0xb: if ((cond&9) == 8 || (cond&9) == 1) bt=1; break; // BLT
            case 0xc: if ((cond&4) == 0 &&
                          ((cond&9) == 9 || (cond&9) == 0)) bt=1; break; // BGT
            case 0xd: if ((cond&4) == 4 ||
                          (cond&9) == 8 || (cond&9) == 1) bt=1; break; // BLE
            case 0xe: bt = 1; // JMP unconditional
            }
         }
         ireg_t naddr;
         if (((inst >> 24) & 0xf) == 0xb) { // func-call
            if (ch == 'n') bt = 0;         // step over function
            else if (bkpt[ci+1] == 0) {    // step into function
               int ret_inst = ptrace(PTRACE_PEEKTEXT, child, addr+4, 0);
               if (bp_toggle(child, &bkpt[ci+1], addr+4, ret_inst, BP_SDB, 0))
                  break;
            }
            else
               bp_info[bkpt[ci+1]-1].flags =
                  (bp_info[bkpt[ci+1]-1].flags + 8) | BP_SDB;
         }
         if (bt) {
            int offset =
               (inst & 0x00ffffff) | ((inst & 0x800000) ? 0xff000000 : 0);
            int *jaddr = (int *)(addr) + 2 + offset;
            naddr = (ireg_t)jaddr;
         }
         else {
            naddr = addr + 4;
         }
         int ninst = ptrace(PTRACE_PEEKTEXT, child, naddr, 0);

         // CLEAR LAST BREAKPOINT (RESTORE HIDDEN INSTRUCTION)
         if (bp_toggle(child, &bkpt[ci], 0, 0, (BP_SDB | BP_USR), 0)) break;

         // CREATE BREAKPOINT AT "NEXT" INSTRUCTION
         int oaddr = addr;
         int oinst = inst;
         addr = naddr;
         inst = ninst;
         ci = (addr - text_base_ptr) / 4;
         if (bp_toggle(child, &bkpt[ci], addr, inst, BP_SDB, 0)) break;

         do {
            if (ptrace(PTRACE_CONT, child, 0, 0) < 0) {
               perror("PTRACE_CONT");
               return;
            }
            waitpid(child, &wait_status, 0);
            estat = (wait_status >> 8) & 0xff;
         } while (estat == SIGWINCH);

         if (((wait_status & 0xff) != 0x7f)) break; // normal termination
         else if (estat != SIGTRAP) { // not a bkpt, so serious error
            if (bp_toggle(child, &bkpt[ci], addr, inst, (BP_SDB | BP_USR), 0))
               break;
            ptrace(PTRACE_GETREGS, child, 0, &iregs);
            addr = iregs.uregs[15];
            ci = (addr - text_base_ptr) / 4;
            if (bkpt[ci] != 0 &&
                bp_toggle(child, &bkpt[ci], addr, inst, (BP_SDB | BP_USR), 0))
               break;
            if (bp_toggle(child, &bkpt[ci], addr, 0, BP_SYS, 1)) break;;
            ll = 0; // facilitate exploration
         }

         // center ASM_BP window at current bkpt
         recenter_inst(&w[ASM_BP], ci);

         ptrace(PTRACE_GETREGS, child, 0, &iregs);
         if (iregs.uregs[11] != 0 && curr_FP != iregs.uregs[11]) {
            curr_FP = iregs.uregs[11];
         }
         redraw = 1;
      }
      else if (ch == 'c' && // 'continue' running until bkpt or termination
               (bp_info[bkpt[ci]-1].flags & BP_SYS) == 0) {

bl_ret:
         // CLEAR LAST BREAKPOINT (RESTORE HIDDEN INSTRUCTION)
         if (bp_toggle(child, &bkpt[ci], 0, 0, (BP_SDB | BP_USR), 0)) break;

         do {
            if (ptrace(PTRACE_CONT, child, 0, 0) < 0) {
               perror("PTRACE_CONT");
               return;
            }
            waitpid(child, &wait_status, 0);
            estat = (wait_status >> 8) & 0xff;
         } while (estat == SIGWINCH);

         ptrace(PTRACE_GETREGS, child, 0, &iregs);
         addr = iregs.uregs[15];
         ci = (addr - text_base_ptr) / 4;

         if (((wait_status & 0xff) != 0x7f)) break; // normal termination
         else if (estat == SIGTRAP) {
            inst = bp_info[bkpt[ci]-1].inst;
            bp_info[bkpt[ci]-1].flags |= BP_SDB;
         }
         else { // not a bkpt, so serious error
            if (bkpt[ci] != 0 &&
                bp_toggle(child, &bkpt[ci], addr,
                          bp_info[bkpt[ci]-1].inst, (BP_SDB | BP_USR), 0))
               break;
            if (bp_toggle(child, &bkpt[ci], addr, 0, BP_SYS, 1)) break;;
            ll = 0; // facilitate exploration
         }

         // center ASM_BP window at current bkpt
         recenter_inst(&w[ASM_BP], ci);

         if (iregs.uregs[11] != 0 && curr_FP != iregs.uregs[11]) {
            curr_FP = iregs.uregs[11];
         }
         redraw = 1;
      }
      else if (fw != &w[ASM_BP]) { // scroll control when not bkpt centered
         if (ch == 'l') ll = 1 - ll; // toggle: lock/float cursor location
         else if (ch == '1') { // go to 'top' of window
            if (fw->uly != lb) {
               fw->uly = lb;
               if ((fw->lly = lb + fw->height-1) >= ub) fw->lly = ub-1;
               redraw = 1;
            }
         }
         else if (ch == 'z') { // go to 'bottom' of window
            if (fw->lly != ub-1) {
               fw->lly = ub-1;
               if ((fw->uly = fw->lly - (fw->height-1)) < lb) fw->uly = lb;
               redraw = 1;
            }
         }
         else if (ch == 'u') { // move up one page
            if (fw->uly != lb) {
               if ((fw->uly -= fw->height) < lb) {
                  fw->uly = lb;
                  fw->lly = lb + (fw->height-1);
                  if (fw->lly >= ub) fw->lly = ub-1;
               }
               else fw->lly -= fw->height;
               redraw = 1;
            }
         } 
         else if (ch == 'd') { // move down one page
            if (fw->lly != ub-1) {
               if ((fw->uly += fw->height) > (ub - fw->height)) {
                  fw->uly = ub - fw->height;
               }
               fw->lly = fw->uly + (fw->height-1);
               if (fw->lly >= ub) fw->lly = ub-1;
               redraw = 1;
            }
         }
         else if (ch == 'j') { // move down one line
            int height = (ub < fw->height) ? ub : (int) fw->height;
            if (ll) {
               if (++fw->uly > (ub - height)) {
                  fw->uly = ub - height;
                  fw->lly = ub-1;
               }
               else if ((fw->lly = fw->uly + height-1) >= ub)
                  fw->lly = ub-1;
               redraw = 1; // this can happen when it doesn't need to
            }
            else if (fw->yoff == (height-1)) {
               if (fw->lly != ub-1) {
                  ++fw->uly;
                  ++fw->lly;
                  redraw = 1;
               }
            }
            else {
               ++fw->yoff;
               redraw = 1;
            }
         }
         else if (ch == 'k') { // move up one line
            if (ll) { // might want to elide the nested if stmt fot better UI
               if (fw->uly > lb) {
                  --fw->uly;
                  if (fw->lly - fw->uly >= fw->height) --fw->lly;
                  redraw = 1;
               }
            }
            else if (fw->yoff == 0) {
               if (fw->uly > lb) {
                  --fw->uly;
                  if (fw->lly - fw->uly >= fw->height) --fw->lly;
                  redraw = 1;
               }
            }
            else {
               --fw->yoff;
               redraw = 1;
            }
         }
         else if (ch == 'b' && fw == &w[ASM_USR]) { // toggle user bkpt
            int inst_idx = fw->uly+fw->yoff;
            if (bp_toggle(child, &bkpt[inst_idx], text_base_ptr + inst_idx*4,
                         strtoul(&text[inst_idx][8], 0, 16), BP_USR, 0)) break;
            redraw = 1;
         }
      }
   }
   while (1);
    
   printf("\e[?25h\n"); // show cursor
   tcsetattr(STDIN_FILENO, TCSANOW, &tty);
   
   if (text) { // free all allocated memory
      for (int i=insts-1; i >= 0; --i) {
         free(text[i]); text[i] = 0;
      }
      free(text); text = 0;
   }

   if ((wait_status & 0x7f) == 0) { // Normal program exit
      printf("program %s ran to completion, exit status = %d\n",
         trace_executable_name, estat | ((estat & 0x80) ? 0xffffff00 : 0));
   }
}

int main(int argc, char **argv) 
{
   pid_t child;

   if (argc < 2) {
      printf("%s <executable to debug>\n", argv[0]);
      return 1;
   }

   int elf_hdr[8];
   int fd = open(argv[1], O_RDONLY);
   if (fd == -1 || read(fd, elf_hdr, 32) != 32) {
      printf("Could not read trace file, %s\n", argv[1]);
      return 1;
   }
   close(fd);

   child = fork();
   if (child == 0) {
      // this scope executes the child process
      if (ptrace(PTRACE_TRACEME, 0, 0, 0) < 0) 
         perror("PTRACE_TRACEME");
      else {
         char *args[8];
         args[0] = (char *)0;
         if (argc < 9) {
            for (int i=2; i<argc; ++i) args[i-2] = argv[i];
            args[argc-2] = (char *)0; // args[7] maximum
         }
         execl(argv[1], argv[1], args[0], args[1], args[2], args[3],
               args[4], args[5], args[6], args[7]);
      }
   } else { 
      debug(child, argv[1], elf_hdr[6]);
   }
   return 0;
}

