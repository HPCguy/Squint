# Squint: A peephole optimizer for stack VM compilers

## Introduction
Short summary: This repo contains a highly functional, but not quite
complete, C compiler (mc.c) that supports JIT execution, ELF executable
generation, and peephole optimization.  mc.c is a follow on to the AMaCC
compiler.  See the AMaCC documentation referenced below for more information.

The MC C compiler found in this repository is a subset of ***the full MC HPC compiler which is
being developed offline.  That said, results from the full MC HPC compiler
are shown [below](#assembly-language-quality)***.

This compiler supports the following features beyond AMaCC:

* ***Float*** data types (AMaCC is an integer based compiler).

* Array declarations and initializers. (e.g. float foo[4][4] = { { 0.0, ... }, { 0.0 ... }, ... };

* The Squint peephole optimizer that ***roughly halves the number of executable
instructions in compiled code***.  The tests/sieve.c
benchmark provides an optimization example, and runs roughly
***4x faster after peephole optimization***.

* Greatly improved type checking, error checking, and IR code generation (try -si option).

Source code size:
* mc C compiler -- 3000 SLOC
* squint optimizer -- 3325 SLOC

The original AMaCC compiler is based on the phenomenal work of the 
team at https://github.com/jserv/amacc , and I strongly suggest
you visit that site to see the history of AMaCC compiler development
as stored in that repository.  It shows how small changes can be
added to a base complier, step-by-step,  to create a truly marvelous
educational compiler. The README there expands upon this README and
is where you will learn the most about the AMaCC compiler that was
used as a starting point for this work.

## Example
The following time command uses the mc compiler to JIT compile an optimized verison
of the mc compiler, using an external optimizer linked as a shared object library,
and then that optimized JIT complier is used to JIT compile an optimized version of
Sieve of Eratosthenes (again using a shared object optimizer), and then the
sieve benchmark is JIT executed.  The time shown is the time it takes to do
everything in this paragraph, sieving 8388608 values using three different
algorithms applied to bit arrays.
```
$ make mc-so      # use gcc to build an enhanced version of the mc compiler
  CC+LD		mc-so
$ time ./mc-so -DSQUINT_SO -Op mc.c -Op tests/sieve.c

real	0m1.031s
user	0m0.999s
sys	0m0.031s
```

## Performance

| Benchmark | AMaCC runtime | Mc+Squint | Gcc | Gcc -O1 | Gcc -03 |
| --- | --- | --- | --- | --- | --- |
| sieve (int) | 3.676s |  ***0.936s*** | 1.642s | 0.942s | 0.962s |
| shock (float) | 39.192s | ***3.047s*** | 9.666s | 4.383s | 3.702s |
| fib 42 (int) | 10.546s | 4.595s | 6.209s | 4.504s | ***3.553s*** |

Note: shock run with 8192 elements, 4096 timesteps, no output. Best of 20 runs.

| Benchmark | AMaCC compile time | Mc+Squint time | Gcc -O3 time |
| --- | --- | --- | --- |
| mc.c | ***0.140s*** | 0.352s | 3.462s |

| Benchmark |  AMaCC .text size | Mc+Squint .text | Gcc -O3 .text | Notes |
| --- | --- | --- | --- | --- |
| bezier.c | 3672 | 1172 | ***768*** | recursive |
| duff.c | 3068 | 564 | ***412*** | unusual |
| maze.c | 6640 | 2632 | ***1752*** | misc |
| shock.c | 8732 | ***1512*** | 3388 | floating point |
| mc.c | 123264 | 61240 | ***34932*** | full compiler |

## Assembly language quality

Below is a comparison of assembly language quality of three compilers on the Raspberry Pi 4B
when compiling tests/shock.c.

* GCC, specifically: "gcc -mfloat-abi=hard -mtune=cortex-a72 -O3 tests/shock.c -lm"

* The Squint compiler uses the compiler in this repository with the -Op option,
followed by the Squint optimizer.

* The MC compiler is a non-public HPC version of Squint that I am working on offline.

For floating point, my HPC compiler is currently always faster than gcc with the above
compiler options, by a minimum of 3%.

That said, make no mistake, my current optimizations are all crap** and yet I am still
beating gcc. I am only one person with no resources, so I  pick the path I see as
most interesting and plod along at a snail's pace when I am not watching TV,
playing video games, or reading/commenting on news articles.

If I had the resources to hire a small team and treat this as a real full time project,
I could do ***much*** better!  If you might be interested, let's talk.

** No common subexpression elimination, register renaming to reduce stalls,
   code motion to reduce stalls, or register coloring to reduce register pressure, etc.
   None of these things are hard to do, but all consume time to implement.

Below is the assembly language for the tests/shock.c ComputeFaceInfo() function for all three compilers:

| gcc | Squint | MC (my HPC compiler) |
| --- | --- | --- |
| ***186++ instructions*** | ***142 instructions*** | ***113 instructions*** |
| ***??? inststructions/iter*** | ***142 instructions/iter*** | ***113 instructions/iter*** |
| 105e0: b  10770 | 5d0: mov  r5, r3 | 5f0: mov  r0, #12 |
| 105e4: mov  r0, r7 | 5d4: add  r6, r3, #1 | 5f4: mla  r0, r3, r0, r4 |
| 105e8: mov  lr, r9 | 5d8: add  r0, r7, r5, lsl #2 | 5f8: vldr  s1, [r0] |
| 105ec: mov  ip, r8 | 5dc: vldr  s1, [r0] | 5fc: vldr  s0, [r0, #12] |
| 105f0: vldr  s22, [lr] | 5e0: add  r0, r7, r6, lsl #2 | 600: vadd.f32  s0, s1, s0 |
| 105f4: vcvt.f64.f32  d7, s21 | 5e4: vldr  s0, [r0] | 604: vmul.f32  s9, s3, s0 |
| 105f8: vadd.f32  s24, s28, s21 | 5e8: vadd.f32  s0, s1, s0 | 608: vldr  s1, [r0, #4] |
| 105fc: vldr  s23, [ip] | 5ec: vmul.f32  s9, s3, s0 | 60c: vldr  s0, [r0, #16] |
| 10600: vldr  s3, [r0] | 5f0: add  r0, r8, r5, lsl #2 | 610: vadd.f32  s0, s1, s0 |
| 10604: vcvt.f64.f32  d2, s22 | 5f4: vldr  s1, [r0] | 614: vmul.f32  s8, s3, s0 |
| 10608: vmul.f64  d7, d7, d6 | 5f8: add  r0, r8, r6, lsl #2 | 618: vldr  s1, [r0, #8] |
| 1060c: vcmpe.f32  s24, #0.0 | 5fc: vldr  s0, [r0] | 61c: vldr  s0, [r0, #20] |
| 10610: vcvt.f64.f32  d3, s23 | 600: vadd.f32  s0, s1, s0 | 620: vadd.f32  s0, s1, s0 |
| 10614: vcvt.f64.f32  d5, s3 | 604: vmul.f32  s8, s3, s0 | 624: vmul.f32  s11, s3, s0 |
| 10618: vmul.f64  d4, d2, d9 | 608: add  r0, r9, r5, lsl #2 | 628: vsub.f32  s6, s2, s4 |
| 1061c: vcvt.f32.f64  s14, d7 | 60c: vldr  s1, [r0] | 62c: vmul.f32  s1, s3, s8 |
| 10620: vmrs  APSR_nzcv, fpscr | 610: add  r0, r9, r6, lsl #2 | 630: vmul.f32  s1, s1, s8 |
| 10624: vmul.f64  d4, d4, d2 | 614: vldr  s0, [r0] | 634: vdiv.f32  s0, s1, s9 |
| 10628: vmul.f32  s23, s23, s14 | 618: vadd.f32  s0, s1, s0 | 638: vsub.f32  s0, s11, s0 |
| 1062c: vmul.f32  s22, s22, s14 | 61c: vmul.f32  s11, s3, s0 | 63c: vmul.f32  s12, s6, s0 |
| 10630: vdiv.f64  d2, d4, d3 | 620: vsub.f32  s6, s2, s4 | 640: vmul.f32  s1, s2, s12 |
| 10634: vstr  s23, [r3] | 624: vmul.f32  s1, s3, s8 | 644: vdiv.f32  s0, s1, s9 |
| 10638: vstr  s22, [r2] | 628: vmul.f32  s1, s1, s8 | 648: vsqrt.f32  s15, s0 |
| 1063c: vsub.f64  d5, d5, d2 | 62c: vdiv.f32  s0, s1, s9 | 64c: vdiv.f32  s13, s8, s9 |
| 10640: vcvt.f32.f64  s10, d5 | 630: vsub.f32  s0, s11, s0 | 650: vcmpe.f32  s13, #0.0 |
| 10644: vsub.f32  s20, s3, s10 | 634: vmul.f32  s12, s6, s0 | 654: vmrs  APSR_nzcv, fpscr |
| 10648: vmul.f32  s20, s20, s14 | 638: vmul.f32  s1, s2, s12 | 658: mov  r0, r3 |
| 1064c: vstr  s20, [r1] | 63c: vdiv.f32  s0, s1, s9 | 65c: bge  0x664 |
| 10650: blt  1081c | 640: vsqrt.f32  s15, s0 | 660: add  r0, r3, #1 |
| 10654: mov  r0, r7 | 644: vdiv.f32  s13, s8, s9 | 664: mov  r5, r0 |
| 10658: mov  lr, r9 | 648: vcmpe.f32  s13, #0.0 | 668: mov  r0, #12 |
| 1065c: mov  ip, r8 | 64c: vmrs  APSR_nzcv, fpscr | 66c: mla  r0, r5, r0, r4 |
| 10660: vldr  s29, [lr] | 650: mov  r0, r5 | 670: vldr  s9, [r0] |
| 10664: vmul.f32  s24, s24, s17 | 654: bge  0x65c | 674: vldr  s8, [r0, #4] |
| 10668: vldr  s27, [ip] | 658: mov  r0, r6 | 678: vldr  s11, [r0, #8] |
| 1066c: vldr  s26, [r0] | 65c: mov  r4, r0 | 67c: vmul.f32  s1, s3, s8 |
| 10670: vcvt.f64.f32  d5, s29 | 660: add  r0, r7, r4, lsl #2 | 680: vmul.f32  s1, s1, s8 |
| 10674: vcvt.f64.f32  d3, s27 | 664: vldr  s9, [r0] | 684: vdiv.f32  s0, s1, s9 |
| 10678: vcvt.f64.f32  d7, s26 | 668: add  r0, r8, r4, lsl #2 | 688: vsub.f32  s12, s11, s0 |
| 1067c: vmul.f64  d4, d5, d9 | 66c: vldr  s8, [r0] | 68c: vsub.f32  s0, s2, s4 |
| 10680: vmul.f64  d5, d4, d5 | 670: add  r0, r9, r4, lsl #2 | 690: vmul.f32  s10, s13, s0 |
| 10684: vdiv.f64  d4, d5, d3 | 674: vldr  s11, [r0] | 694: vmul.f32  s16, s10, s9 |
| 10688: vsub.f64  d7, d7, d4 | 678: vmul.f32  s1, s3, s8 | 698: vmul.f32  s17, s10, s8 |
| 1068c: vmul.f64  d7, d7, d6 | 67c: vmul.f32  s1, s1, s8 | 69c: vsub.f32  s0, s11, s12 |
| 10690: vcvt.f32.f64  s14, d7 | 680: vdiv.f32  s0, s1, s9 | 6a0: vmul.f32  s18, s10, s0 |
| 10694: vmul.f32  s15, s14, s25 | 684: vsub.f32  s12, s11, s0 | 6a4: vadd.f32  s1, s13, s15 |
| 10698: vdiv.f32  s0, s15, s27 | 688: vsub.f32  s0, s2, s4 | 6a8: vcmpe.f32  s1, #0.0 |
| 1069c: vcmp.f32  s0, #0.0 | 68c: vmul.f32  s10, s13, s0 | 6ac: vmrs  APSR_nzcv, fpscr |
| 106a0: vsqrt.f32  s15, s0 | 690: vmul.f32  s16, s10, s9 | 6b0: mov  r0, r3 |
| 106a4: vmrs  APSR_nzcv, fpscr | 694: ldr  r2, [fp, #12] | 6b4: bge  0x6bc |
| 106a8: bmi  10b48 | 698: add  r1, r2, r3, lsl #2 | 6b8: add  r0, r3, #1 |
| 106ac: vadd.f32  s26, s26, s14 | 69c: vmul.f32  s0, s10, s8 | 6bc: mov  r5, r0 |
| 106b0: vmov.f32  s14, s29 | 6a0: vstr  s0, [r1] | 6c0: mov  r0, #12 |
| 106b4: vsub.f32  s21, s21, s28 | 6a4: ldr  r2, [fp, #8] | 6c4: mla  r0, r5, r0, r4 |
| 106b8: vmla.f32  s23, s27, s24 | 6a8: add  r1, r2, r3, lsl #2 | 6c8: vldr  s9, [r0] |
| 106bc: vmla.f32  s14, s27, s15 | 6ac: vsub.f32  s0, s11, s12 | 6cc: vldr  s8, [r0, #4] |
| 106c0: vmla.f32  s26, s29, s15 | 6b0: vmul.f32  s0, s10, s0 | 6d0: vldr  s11, [r0, #8] |
| 106c4: vcmpe.f32  s21, #0.0 | 6b4: vstr  s0, [r1] | 6d4: vsub.f32  s6, s2, s4 |
| 106c8: vmla.f32  s22, s14, s24 | 6b8: vadd.f32  s1, s13, s15 | 6d8: vmul.f32  s1, s3, s8 |
| 106cc: vmla.f32  s20, s26, s24 | 6bc: vcmpe.f32  s1, #0.0 | 6dc: vmul.f32  s1, s1, s8 |
| 106d0: vstr  s23, [r3] | 6c0: vmrs  APSR_nzcv, fpscr | 6e0: vdiv.f32  s0, s1, s9 |
| 106d4: vmrs  APSR_nzcv, fpscr | 6c4: mov  r0, r5 | 6e4: vsub.f32  s0, s11, s0 |
| 106d8: vstr  s22, [r2] | 6c8: bge  0x6d0 | 6e8: vmul.f32  s12, s6, s0 |
| 106dc: vstr  s20, [r1] | 6cc: mov  r0, r6 | 6ec: vadd.f32  s0, s13, s15 |
| 106e0: bge  106f0 | 6d0: mov  r4, r0 | 6f0: vmul.f32  s10, s3, s0 |
| 106e4: mov  r7, r6 | 6d4: add  r0, r7, r4, lsl #2 | 6f4: vmul.f32  s1, s2, s12 |
| 106e8: mov  r9, r5 | 6d8: vldr  s9, [r0] | 6f8: vdiv.f32  s0, s1, s9 |
| 106ec: mov  r8, r4 | 6dc: add  r0, r8, r4, lsl #2 | 6fc: vsqrt.f32  s14, s0 |
| 106f0: vldr  s27, [r9] | 6e0: vldr  s8, [r0] | 700: vmla.f32  s16, s10, s9 |
| 106f4: vmul.f32  s21, s21, s17 | 6e4: add  r0, r9, r4, lsl #2 | 704: vmul.f32  s0, s9, s14 |
| 106f8: vldr  s26, [r8] | 6e8: vldr  s11, [r0] | 708: vadd.f32  s0, s8, s0 |
| 106fc: vldr  s24, [r7] | 6ec: vsub.f32  s6, s2, s4 | 70c: vmla.f32  s17, s10, s0 |
| 10700: vcvt.f64.f32  d5, s27 | 6f0: vmul.f32  s1, s3, s8 | 710: vadd.f32  s5, s11, s12 |
| 10704: vcvt.f64.f32  d3, s26 | 6f4: vmul.f32  s1, s1, s8 | 714: vmla.f32  s5, s8, s14 |
| 10708: vcvt.f64.f32  d7, s24 | 6f8: vdiv.f32  s0, s1, s9 | 718: vmla.f32  s18, s10, s5 |
| 1070c: vmul.f64  d4, d5, d9 | 6fc: vsub.f32  s0, s11, s0 | 71c: vsub.f32  s1, s13, s15 |
| 10710: vmul.f64  d5, d4, d5 | 700: vmul.f32  s12, s6, s0 | 720: vcmpe.f32  s1, #0.0 |
| 10714: vdiv.f64  d4, d5, d3 | 704: vadd.f32  s0, s13, s15 | 724: vmrs  APSR_nzcv, fpscr |
| 10718: vsub.f64  d7, d7, d4 | 708: vmul.f32  s10, s3, s0 | 728: mov  r0, r3 |
| 1071c: vmul.f64  d7, d7, d6 | 70c: vmul.f32  s1, s2, s12 | 72c: bge  0x734 |
| 10720: vcvt.f32.f64  s14, d7 | 710: vdiv.f32  s0, s1, s9 | 730: add  r0, r3, #1 |
| 10724: vmul.f32  s15, s14, s25 | 714: vsqrt.f32  s14, s0 | 734: mov  r5, r0 |
| 10728: vdiv.f32  s0, s15, s26 | 718: vmla.f32  s16, s10, s9 | 738: mov  r0, #12 |
| 1072c: vcmp.f32  s0, #0.0 | 71c: ldr  r2, [fp, #12] | 73c: mla  r0, r5, r0, r4 |
| 10730: vsqrt.f32  s28, s0 | 720: add  r0, r2, r3, lsl #2 | 740: vldr  s9, [r0] |
| 10734: vmrs  APSR_nzcv, fpscr | 724: vldr  s7, [r0] | 744: vldr  s8, [r0, #4] |
| 10738: bmi  10b14 | 728: vmul.f32  s0, s9, s14 | 748: vldr  s11, [r0, #8] |
| 1073c: vadd.f32  s24, s24, s14 | 72c: vadd.f32  s0, s8, s0 | 74c: vsub.f32  s6, s2, s4 |
| 10740: vmov.f32  s15, s27 | 730: vmla.f32  s7, s10, s0 | 750: vmul.f32  s1, s3, s8 |
| 10744: ldr  r0, [sp, #52]  ; 0x34 | 734: vstr  s7, [r0] | 754: vmul.f32  s1, s1, s8 |
| 10748: vmla.f32  s23, s26, s21 | 738: ldr  r2, [fp, #8] | 758: vdiv.f32  s0, s1, s9 |
| 1074c: vmls.f32  s15, s26, s28 | 73c: add  r0, r2, r3, lsl #2 | 75c: vsub.f32  s0, s11, s0 |
| 10750: cmp  r0, r4 | 740: vldr  s7, [r0] | 760: vmul.f32  s12, s6, s0 |
| 10754: vmls.f32  s24, s27, s28 | 744: vadd.f32  s5, s11, s12 | 764: vsub.f32  s0, s13, s15 |
| 10758: vmla.f32  s22, s15, s21 | 748: vmla.f32  s5, s8, s14 | 768: vmul.f32  s10, s3, s0 |
| 1075c: vmla.f32  s20, s24, s21 | 74c: vmla.f32  s7, s10, s5 | 76c: vmul.f32  s1, s2, s12 |
| 10760: vstmia  r3!, {s23} | 750: vstr  s7, [r0] | 770: vdiv.f32  s0, s1, s9 |
| 10764: vstmia  r2!, {s22} | 754: vsub.f32  s1, s13, s15 | 774: vsqrt.f32  s14, s0 |
| 10768: vstmia  r1!, {s20} | 758: vcmpe.f32  s1, #0.0 | 778: mov  r0, #12 |
| 1076c: beq  1085c ***EXIT LOOP*** | 75c: vmrs  APSR_nzcv, fpscr | 77c: mla  r0, r3, r0, r6 |
| 10770: mov  r9, r5 | 760: mov  r0, r5 | 780: vmla.f32  s16, s10, s9 |
| 10774: add  r5, r5, #4 | 764: bge  0x76c | 784: vstr  s16, [r0] |
| 10778: vldr  s20, [r4] | 768: mov  r0, r6 | 788: vmul.f32  s0, s9, s14 |
| 1077c: mov  r8, r4 | 76c: mov  r4, r0 | 78c: vsub.f32  s0, s8, s0 |
| 10780: add  r4, r4, #4 | 770: add  r0, r7, r4, lsl #2 | 790: vmla.f32  s17, s10, s0 |
| 10784: vldr  s15, [r5] | 774: vldr  s9, [r0] | 794: vstr  s17, [r0, #4] |
| 10788: mov  r7, r6 | 778: add  r0, r8, r4, lsl #2 | 798: vadd.f32  s5, s11, s12 |
| 1078c: add  r6, r6, #4 | 77c: vldr  s8, [r0] | 79c: vmls.f32  s5, s8, s14 |
| 10790: vldr  s22, [r9] | 780: add  r0, r9, r4, lsl #2 | 7a0: vmla.f32  s18, s10, s5 |
| 10794: vldr  s11, [r4] | 784: vldr  s11, [r0] | 7a4: vstr  s18, [r0, #8] |
| 10798: vldr  s14, [r6, #-4] | 788: vsub.f32  s6, s2, s4 | 7a8: add  r3, r3, #1 |
| 1079c: vadd.f32  s22, s22, s15 | 78c: vmul.f32  s1, s3, s8 | 7ac: cmp  r3, r7 |
| 107a0: vldr  d6, [pc, #144] | 790: vmul.f32  s1, s1, s8 | 7b0: blt  0x5f0 |
| 107a4: vadd.f32  s20, s20, s11 | 794: vdiv.f32  s0, s1, s9 | |
| 107a8: vldr  s15, [r6] | 798: vsub.f32  s0, s11, s0 | |
| 107ac: vmul.f32  s22, s22, s17 | 79c: vmul.f32  s12, s6, s0 | |
| 107b0: vsub.f64  d6, d15, d6 | 7a0: vsub.f32  s0, s13, s15 | |
| 107b4: vmul.f32  s20, s20, s17 | 7a4: vmul.f32  s10, s3, s0 | |
| 107b8: vadd.f32  s14, s14, s15 | 7a8: vmul.f32  s1, s2, s12 | |
| 107bc: vcvt.f64.f32  d5, s22 | 7ac: vdiv.f32  s0, s1, s9 | |
| 107c0: vcvt.f64.f32  d3, s20 | 7b0: vsqrt.f32  s14, s0 | |
| 107c4: vmul.f32  s14, s14, s17 | 7b4: add  r0, sl, r3, lsl #2 | |
| 107c8: vmul.f64  d4, d5, d9 | 7b8: vmla.f32  s16, s10, s9 | |
| 107cc: vcvt.f64.f32  d7, s14 | 7bc: vstr  s16, [r0] | |
| 107d0: vmul.f64  d4, d4, d5 | 7c0: ldr  r2, [fp, #12] | |
| 107d4: vdiv.f64  d5, d4, d3 | 7c4: add  r0, r2, r3, lsl #2 | |
| 107d8: vsub.f64  d7, d7, d5 | 7c8: vldr  s7, [r0] | |
| 107dc: vmul.f64  d7, d7, d6 | 7cc: vmul.f32  s0, s9, s14 | |
| 107e0: vcvt.f32.f64  s14, d7 | 7d0: vsub.f32  s0, s8, s0 | |
| 107e4: vmul.f32  s14, s14, s25 | 7d4: vmla.f32  s7, s10, s0 | |
| 107e8: vdiv.f32  s0, s14, s20 | 7d8: vstr  s7, [r0] | |
| 107ec: vcmp.f32  s0, #0.0 | 7dc: ldr  r2, [fp, #8] | |
| 107f0: vsqrt.f32  s28, s0 | 7e0: add  r0, r2, r3, lsl #2 | |
| 107f4: vmrs  APSR_nzcv, fpscr | 7e4: vldr  s7, [r0] | |
| 107f8: bmi  10ae8 | 7e8: vadd.f32  s5, s11, s12 | |
| 107fc: vdiv.f32  s21, s22, s20 | 7ec: vmls.f32  s5, s8, s14 | |
| 10800: vcmpe.f32  s21, #0.0 | 7f0: vmla.f32  s7, s10, s5 | |
| 10804: vmrs  APSR_nzcv, fpscr | 7f4: vstr  s7, [r0] | |
| 10808: bge  105e4 | 7f8: add  r3, r3, #1 | |
| 1080c: mov  r0, r6 | 7fc: ldr  r0, [fp, #32] | |
| 10810: mov  lr, r5 | 800: cmp  r3, r0 | |
| 10814: mov  ip, r4 | 804: blt  0x5d0 | |
| 10818: b  105f0 | | |
| 1081c: mov  r0, r6 | | |
| 10820: mov  lr, r5 | | |
| 10824: mov  ip, r4 | | |
| 10828: b  10660 | | |
| 10ae8: str  r1, [sp, #84] | | |
| 10aec: strd  r2, [sp, #88] | | |
| 10af0: bl  1043c <sqrtf@plt> | | |
| 10af4: ldr  r3, [pc, #-676] | | |
| 10af8: vldr  d7, [pc, #136] | | |
| 10afc: ldr  r1, [sp, #84] | | |
| 10b00: vldr  s25, [r3, #4] | | |
| 10b04: ldrd  r2, [sp, #88] | | |
| 10b08: vcvt.f64.f32  d15, s25 | | |
| 10b0c: vsub.f64  d6, d15, d7 | | |
| 10b10: b  107fc | | |
| 10b14: vstr  s14, [sp, #84] | | |
| 10b18: str  r1, [sp, #88] | | |
| 10b1c: strd  r2, [sp, #92] | | |
| 10b20: bl  1043c <sqrtf@plt> | | |
| 10b24: ldr  r3, [pc, #100] | | |
| 10b28: vldr  d7, [pc, #88] | | |
| 10b2c: ldr  r1, [sp, #88] | | |
| 10b30: vldr  s25, [r3, #4] | | |
| 10b34: ldrd  r2, [sp, #92] | | |
| 10b38: vcvt.f64.f32  d15, s25 | | |
| 10b3c: vsub.f64  d6, d15, d7 | | |
| 10b40: vldr  s14, [sp, #84] | | |
| 10b44: b  1073c | | |
| 10b48: vstr  s14, [sp, #84] | | |
| 10b4c: vstr  s15, [sp, #88] | | |
| 10b50: str  r1, [sp, #92] | | |
| 10b54: strd  r2, [sp, #96] | | |
| 10b58: bl  1043c <sqrtf@plt> | | |
| 10b5c: ldr  r3, [pc, #44] | | |
| 10b60: vldr  d7, [pc, #32] | | |
| 10b64: ldr  r1, [sp, #92] | | |
| 10b68: vldr  s25, [r3, #4] | | |
| 10b6c: ldrd  r2, [sp, #96] | | |
| 10b70: vcvt.f64.f32  d15, s25 | | |
| 10b74: vsub.f64  d6, d15, d7 | | |
| 10b78: vldr  s14, [sp, #84] | | |
| 10b7c: vldr  s15, [sp, #88] | | |
| 10b80: b  106ac | | |

By the end of 2022, I expect to have automatic vectorization and/or parallelization working
in my offline HPC compiler.  The HPC extensions/restrictions make it "natural" to manage
parallel partitions, unlike the mess created by C standard semantics.

## Prerequisites
* This compiler project depends on several GNU/Linux behaviors, and it
  is necessary to have Arm/Linux installed in your build environment.
* Install [GNU Toolchain for the A-profile Architecture](https://developer.arm.com/tools-and-software/open-source-software/developer-tools/gnu-toolchain/gnu-a/downloads)
    - Select `arm-linux-none-gnueabihf` (AArch32 target with hard float)

## Test environment:
* Raspberry Pi 4B (SoC: bcm2711, ARMv8-A architecture)
* Raspbian GNU/Linux, kernel 5.10.17-v7l+, gcc 8.3.0 (armv7l userland)

The architecture support targets armv7hf with Linux ABI, verified on
Raspberry Pi 2/3/4 with GNU/Linux.

## Acknowledgements
AMaCC is based on the infrastructure of [c4](https://github.com/rswier/c4).
Please see the AMaCC repository [AMaCC](https://github.com/jserv/amacc) to
learn more about the AMaCC compiler.
