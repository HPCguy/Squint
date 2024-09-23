# Squint: A peephole optimizer for stack VM compilers

## Introduction
Short summary: This repo contains a highly functional, but not quite
complete, C compiler (mc.c) that supports JIT execution, ELF executable
generation, and peephole optimization.  mc.c is a follow on to the AMaCC
compiler.  See the AMaCC documentation referenced below for more information.

This compiler was developed on a Raspberry Pi computer running 32 bit Buster Linux.
See the "Discussion" tab above for instructions on using this compiler
with an ARM Chromebook.

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

Source code size (public code version, this repository):
* mc C compiler -- 4035 SLOC
* squint optimizer -- 3836 SLOC

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

| Benchmark | Mc runtime | Mc+Squint | Gcc | Gcc -O1 | Gcc -03 |
| --- | --- | --- | --- | --- | --- |
| sieve (int) | 3.676s |  ***0.936s*** | 1.642s | 0.942s | 0.962s |
| shock (float) | 39.192s | ***3.125s*** | 9.346s | 4.092s | 3.217s |
| nbody_arr (float) | 40.43s | 3.29s | 12.28s | 3.25s | ***3.21s*** |

Note: shock run with 8192 elements, 4096 timesteps, no output. Best of 20 runs.

| Benchmark | Mc compile time | Mc+Squint time | Gcc -O3 time |
| --- | --- | --- | --- |
| mc.c | ***0.140s*** | 0.919s | 6.71s |

| Benchmark |  Mc .text size | Mc+Squint .text | Gcc -O3 .text | Notes |
| --- | --- | --- | --- | --- |
| bezier.c | 3376 | 1020 | ***768*** | recursive |
| duff.c | 2972 | 520 | ***412*** | unusual |
| maze.c | 6568 | 2488 | ***1752*** | misc |
| shock.c | 7844 | ***2112*** | 3388 | floating point |
| mc.c | 183248 | 85568 | ***59124*** | full compiler |

## Assembly language quality

Below is a comparison of assembly language quality of three compilers on the Raspberry Pi 4B
when compiling tests/shock.c.

* GCC, specifically: "gcc -mfloat-abi=hard -mtune=cortex-a72 -O3 tests/shock.c -lm"

* The Squint compiler uses the compiler in ***this repository*** with the -Op option,
followed by the Squint optimizer.

* The MC compiler is a non-public HPC version of Squint that I am working on ***offline***.

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

Below is a C function followed by the assembly language listing for the loop body,
for all three compilers. The MC compiler creates only 3.05 assembly language
instructions to represent each (complex) line of high level C code, on average:

```
void ComputeFaceInfo(int numFace, float *mass, float *momentum, float *energy,
                                  float *f0, float *f1, float *f2)
{
   int i;
   int contributor;
   float ev;
   float cLocal;

   for (i = 0; i < numFace; ++i)
   {
      /* each face has an upwind and downwind element. */
      int upWind   = i;     /* upwind element */
      int downWind = i + 1; /* downwind element */

      /* calculate face centered quantities */
      float massf =     0.5 * (mass[upWind]     + mass[downWind]);
      float momentumf = 0.5 * (momentum[upWind] + momentum[downWind]);
      float energyf =   0.5 * (energy[upWind]   + energy[downWind]);
      float pressuref = (gammaa - 1.0) *
                         (energyf - 0.5*momentumf*momentumf/massf);
      float c = sqrtf(gammaa*pressuref/massf);
      float v = momentumf/massf;

      /* Now that we have the wave speeds, we might want to */
      /* look for the max wave speed here, and update dt */
      /* appropriately right before leaving this function. */
      /* ... */

      /* OK, calculate face quantities */

      contributor = ((v >= 0.0) ? upWind : downWind);
      massf = mass[contributor];
      momentumf = momentum[contributor];
      energyf = energy[contributor];
      pressuref = energyf - 0.5*momentumf*momentumf/massf;
      ev = v*(gammaa - 1.0);

      f0[i] = ev*massf;
      f1[i] = ev*momentumf;
      f2[i] = ev*(energyf - pressuref);

      contributor = ((v + c >= 0.0) ? upWind : downWind);
      massf = mass[contributor];
      momentumf = momentum[contributor];
      energyf = energy[contributor];
      pressuref = (gammaa - 1.0)*(energyf - 0.5*momentumf*momentumf/massf);
      ev = 0.5*(v + c);
      cLocal = sqrtf(gammaa*pressuref/massf);

      f0[i] += ev*massf;
      f1[i] += ev*(momentumf + massf*cLocal);
      f2[i] += ev*(energyf + pressuref + momentumf*cLocal);

      contributor = ((v - c >= 0.0) ? upWind : downWind);
      massf = mass[contributor];
      momentumf = momentum[contributor];
      energyf = energy[contributor];
      pressuref = (gammaa - 1.0)*(energyf - 0.5*momentumf*momentumf/massf);
      ev = 0.5*(v - c);
      cLocal = sqrtf(gammaa*pressuref/massf);

      f0[i] += ev*massf;
      f1[i] += ev*(momentumf - massf*cLocal);
      f2[i] += ev*(energyf + pressuref - momentumf*cLocal);
   }
}
```

| gcc | Squint (this repo) | MC (private repo HPC compiler) |
| --- | --- | --- |
| ***164++ instructions*** | ***119 instructions*** | ***113 instructions*** |
| ***??? inststructions/iter*** | ***119 instructions/iter*** | ***113 instructions/iter*** |
| 10d00: ea000060 b 10e88 | 640: add  r0, r5, r3, lsl #2 | 5f0: mov  r0, #12 |
| 10d04: e1a00008 mov r0, r8 | 644: vldr  s1, [r0] | 5f4: mla  r0, r3, r0, r4 |
| 10d08: e1a0c00a mov ip, sl | 648: vldr  s0, [r0, #4] | 5f8: vldr  s1, [r0] |
| 10d0c: e1a03009 mov r3, r9 | 64c: vadd.f32  s0, s1, s0 | 5fc: vldr  s0, [r0, #12] |
| 10d10: eddc6a00 vldr s13, [ip] | 650: vmul.f32  s10, s3, s0 | 600: vadd.f32  s0, s1, s0 |
| 10d14: ee687a07 vmul.f32 s15, s16, s14 | 654: add  r0, r6, r3, lsl #2 | 604: vmul.f32  s9, s3, s0 |
| 10d18: ee3c9a08 vadd.f32 s18, s24, s16 | 658: vldr  s1, [r0] | 608: vldr  s1, [r0, #4] |
| 10d1c: e58d5004 str r5, [sp, #4] | 65c: vldr  s0, [r0, #4] | 60c: vldr  s0, [r0, #16] |
| 10d20: edd35a00 vldr s11, [r3] | 660: vadd.f32  s0, s1, s0 | 610: vadd.f32  s0, s1, s0 |
| 10d24: e1a03004 mov r3, r4 | 664: vmul.f32  s9, s3, s0 | 614: vmul.f32  s8, s3, s0 |
| 10d28: ed907a00 vldr s14, [r0] | 668: add  r0, r7, r3, lsl #2 | 618: vldr  s1, [r0, #8] |
| 10d2c: ee266aa8 vmul.f32 s12, s13, s17 | 66c: vldr  s1, [r0] | 61c: vldr  s0, [r0, #20] |
| 10d30: ee265aa7 vmul.f32 s10, s13, s15 | 670: vldr  s0, [r0, #4] | 620: vadd.f32  s0, s1, s0 |
| 10d34: eeb59ac0 vcmpe.f32 s18, #0.0 | 674: vadd.f32  s0, s1, s0 | 624: vmul.f32  s11, s3, s0 |
| 10d38: ee654aa7 vmul.f32 s9, s11, s15 | 678: vmul.f32  s12, s3, s0 | 628: vsub.f32  s6, s2, s4 |
| 10d3c: ee266a26 vmul.f32 s12, s12, s13 | 67c: vsub.f32  s7, s2, s4 | 62c: vmul.f32  s1, s3, s8 |
| 10d40: eef1fa10 vmrs APSR_nzcv, fpscr | 680: vmul.f32  s1, s3, s9 | 630: vmul.f32  s1, s1, s8 |
| 10d44: edc64a00 vstr s9, [r6] | 684: vmul.f32  s1, s1, s9 | 634: vdiv.f32  s0, s1, s9 |
| 10d48: ed855a00 vstr s10, [r5] | 688: vdiv.f32  s0, s1, s10 | 638: vsub.f32  s0, s11, s0 |
| 10d4c: eec66a25 vdiv.f32 s13, s12, s11 | 68c: vsub.f32  s0, s12, s0 | 63c: vmul.f32  s12, s6, s0 |
| 10d50: ee776a66 vsub.f32 s13, s14, s13 | 690: vmul.f32  s13, s7, s0 | 640: vmul.f32  s1, s2, s12 |
| 10d54: ee377a66 vsub.f32 s14, s14, s13 | 694: vmul.f32  s1, s2, s13 | 644: vdiv.f32  s0, s1, s9 |
| 10d58: ee677a27 vmul.f32 s15, s14, s15 | 698: vdiv.f32  s0, s1, s10 | 648: vsqrt.f32  s15, s0 |
| 10d5c: edc47a00 vstr s15, [r4] | 69c: vsqrt.f32  s16, s0 | 64c: vdiv.f32  s13, s8, s9 |
| 10d60: ba00006e blt 10f20 | 6a0: vdiv.f32  s14, s9, s10 | 650: vcmpe.f32  s13, #0.0 |
| 10d64: e1a00008 mov r0, r8 | 6a4: vcmpe.f32  s14, s5 | 654: vmrs  APSR_nzcv, fpscr |
| 10d68: e1a0e00a mov lr, sl | 6a8: vmrs APSR_nzcv, fpscr | 658: mov  r0, r3 |
| 10d6c: e1a0c009 mov ip, r9 | 6ac: mov  r0, r3 | 65c: bge  0x664 |
| 10d70: ed9eba00 vldr s22, [lr] | 6b0: bge 0x6b8 | 660: add  r0, r3, #1 |
| 10d74: ee299a28 vmul.f32 s18, s18, s17 | 6b4: add  r0, r3, #1 | 664: mov  r5, r0 |
| 10d78: eddcba00 vldr s23, [ip] | 6b8: mov  r4, r0 | 668: mov  r0, #12 |
| 10d7c: edd09a00 vldr s19, [r0] | 6bc: add  r0, r5, r4, lsl #2 | 66c: mla  r0, r5, r0, r4 |
| 10d80: ee2b7a28 vmul.f32 s14, s22, s17 | 6c0: vldr  s10, [r0] | 670: vldr  s9, [r0] |
| 10d84: eddb7a01 vldr s15, [fp, #4] | 6c4: add  r0, r6, r4, lsl #2 | 674: vldr  s8, [r0, #4] |
| 10d88: ee277a0b vmul.f32 s14, s14, s22 | 6c8: vldr  s9, [r0] | 678: vldr  s11, [r0, #8] |
| 10d8c: ee776aea vsub.f32 s13, s15, s21 | 6cc: add  r0, r7, r4, lsl #2 | 67c: vmul.f32  s1, s3, s8 |
| 10d90: ee87aa2b vdiv.f32 s20, s14, s23 | 6d0: vldr  s12, [r0] | 680: vmul.f32  s1, s1, s8 |
| 10d94: ee39aaca vsub.f32 s20, s19, s20 | 6d4: vmul.f32  s1, s3, s9 | 684: vdiv.f32  s0, s1, s9 |
| 10d98: ee2aaa26 vmul.f32 s20, s20, s13 | 6d8: vmul.f32  s1, s1, s9 | 688: vsub.f32  s12, s11, s0 |
| 10d9c: ee677a8a vmul.f32 s15, s15, s20 | 6dc: vdiv.f32  s0, s1, s10 | 68c: vsub.f32  s0, s2, s4 |
| 10da0: ee870aab vdiv.f32 s0, s15, s23 | 6e0: vsub.f32  s13, s12, s0 | 690: vmul.f32  s10, s13, s0 |
| 10da4: eeb50a40 vcmp.f32 s0, #0.0 | 6e4: vsub.f32  s0, s2, s4 | 694: vmul.f32  s16, s10, s9 |
| 10da8: eef1cac0 vsqrt.f32 s25, s0 | 6e8: vmul.f32  s11, s14, s0 | 698: vmul.f32  s17, s10, s8 |
| 10dac: eef1fa10 vmrs APSR_nzcv, fpscr | 6ec: vmul.f32  s17, s11, s10 | 69c: vsub.f32  s0, s11, s12 |
| 10db0: 4a000076 bmi 10f90 | 6f0: vmul.f32  s18, s11, s9 | 6a0: vmul.f32  s18, s10, s0 |
| 10db4: edd67a00 vldr s15, [r6] | 6f4: vsub.f32  s0, s12, s13 | 6a4: vadd.f32  s1, s13, s15 |
| 10db8: eeb07a4b vmov.f32 s14, s22 | 6f8: vmul.f32  s19, s11, s0 | 6a8: vcmpe.f32  s1, #0.0 |
| 10dbc: ee799a8a vadd.f32 s19, s19, s20 | 6fc: vadd.f32  s1, s14, s16 | 6ac: vmrs  APSR_nzcv, fpscr |
| 10dc0: ee388a4c vsub.f32 s16, s16, s24 | 700: vcmpe.f32  s1, s5 | 6b0: mov  r0, r3 |
| 10dc4: ee0b7aac vmla.f32 s14, s23, s25 | 704: vmrs APSR_nzcv, fpscr | 6b4: bge  0x6bc |
| 10dc8: ee4b7a89 vmla.f32 s15, s23, s18 | 708: mov  r0, r3 | 6b8: add  r0, r3, #1 |
| 10dcc: ee4b9a2c vmla.f32 s19, s22, s25 | 70c: bge 0x714 | 6bc: mov  r5, r0 |
| 10dd0: eeb58ac0 vcmpe.f32 s16, #0.0 | 710: add  r0, r3, #1 | 6c0: mov  r0, #12 |
| 10dd4: eef1fa10 vmrs APSR_nzcv, fpscr | 714: mov  r4, r0 | 6c4: mla  r0, r5, r0, r4 |
| 10dd8: edc67a00 vstr s15, [r6] | 718: add  r0, r5, r4, lsl #2 | 6c8: vldr  s9, [r0] |
| 10ddc: edd57a00 vldr s15, [r5] | 71c: vldr  s10, [r0] | 6cc: vldr  s8, [r0, #4] |
| 10de0: ee477a09 vmla.f32 s15, s14, s18 | 720: add  r0, r6, r4, lsl #2 | 6d0: vldr  s11, [r0, #8] |
| 10de4: edc57a00 vstr s15, [r5] | 724: vldr  s9, [r0] | 6d4: vsub.f32  s6, s2, s4 |
| 10de8: edd47a00 vldr s15, [r4] | 728: add  r0, r7, r4, lsl #2 | 6d8: vmul.f32  s1, s3, s8 |
| 10dec: ee497a89 vmla.f32 s15, s19, s18 | 72c: vldr  s12, [r0] | 6dc: vmul.f32  s1, s1, s8 |
| 10df0: edc47a00 vstr s15, [r4] | 730: vsub.f32  s7, s2, s4 | 6e0: vdiv.f32  s0, s1, s9 |
| 10df4: aa000002 bge 10e04 | 734: vmul.f32  s1, s3, s9 | 6e4: vsub.f32  s0, s11, s0 |
| 10df8: e1a08001 mov r8, r1 | 738: vmul.f32  s1, s1, s9 | 6e8: vmul.f32  s12, s6, s0 |
| 10dfc: e1a0a002 mov sl, r2 | 73c: vdiv.f32  s0, s1, s10 | 6ec: vadd.f32  s0, s13, s15 |
| 10e00: e1a09007 mov r9, r7 | 740: vsub.f32  s0, s12, s0 | 6f0: vmul.f32  s10, s3, s0 |
| 10e04: ed9aaa00 vldr s20, [sl] | 744: vmul.f32  s13, s7, s0 | 6f4: vmul.f32  s1, s2, s12 |
| 10e08: ee288a28 vmul.f32 s16, s16, s17 | 748: vadd.f32  s0, s14, s16 | 6f8: vdiv.f32  s0, s1, s9 |
| 10e0c: ed99ba00 vldr s22, [r9] | 74c: vmul.f32  s11, s3, s0 | 6fc: vsqrt.f32  s14, s0 |
| 10e10: ed989a00 vldr s18, [r8] | 750: vmul.f32  s1, s2, s13 | 700: vmla.f32  s16, s10, s9 |
| 10e14: ee2a7a28 vmul.f32 s14, s20, s17 | 754: vdiv.f32  s0, s1, s10 | 704: vmul.f32  s0, s9, s14 |
| 10e18: eddb7a01 vldr s15, [fp, #4] | 758: vsqrt.f32  s15, s0 | 708: vadd.f32  s0, s8, s0 |
| 10e1c: ee277a0a vmul.f32 s14, s14, s20 | 75c: vmla.f32  s17, s11, s10 | 70c: vmla.f32  s17, s10, s0 |
| 10e20: ee776aea vsub.f32 s13, s15, s21 | 760: vmul.f32  s0, s10, s15 | 710: vadd.f32  s5, s11, s12 |
| 10e24: eec79a0b vdiv.f32 s19, s14, s22 | 764: vadd.f32  s0, s9, s0 | 714: vmla.f32  s5, s8, s14 |
| 10e28: ee799a69 vsub.f32 s19, s18, s19 | 768: vmla.f32  s18, s11, s0 | 718: vmla.f32  s18, s10, s5 |
| 10e2c: ee699aa6 vmul.f32 s19, s19, s13 | 76c: vadd.f32  s6, s12, s13 | 71c: vsub.f32  s1, s13, s15 |
| 10e30: ee677aa9 vmul.f32 s15, s15, s19 | 770: vmla.f32  s6, s9, s15 | 720: vcmpe.f32  s1, #0.0 |
| 10e34: ee870a8b vdiv.f32 s0, s15, s22 | 774: vmla.f32  s19, s11, s6 | 724: vmrs  APSR_nzcv, fpscr |
| 10e38: eeb50a40 vcmp.f32 s0, #0.0 | 778: vsub.f32  s1, s14, s16 | 728: mov  r0, r3 |
| 10e3c: eef1bac0 vsqrt.f32 s23, s0 | 77c: vcmpe.f32  s1, s5 | 72c: bge  0x734 |
| 10e40: eef1fa10 vmrs APSR_nzcv, fpscr | 780: vmrs APSR_nzcv, fpscr | 730: add  r0, r3, #1 |
| 10e44: 4a000049 bmi 10f70 | 784: mov  r0, r3 | 734: mov  r5, r0 |
| 10e48: edd67a00 vldr s15, [r6] | 788: bge 0x790 | 738: mov  r0, #12 |
| 10e4c: eeb07a4a vmov.f32 s14, s20 | 78c: add  r0, r3, #1 | 73c: mla  r0, r5, r0, r4 |
| 10e50: ee399a29 vadd.f32 s18, s18, s19 | 790: mov  r4, r0 | 740: vldr  s9, [r0] |
| 10e54: e59d0000 ldr r0, [sp] | 794: add  r0, r5, r4, lsl #2 | 744: vldr  s8, [r0, #4] |
| 10e58: ee0b7a6b vmls.f32 s14, s22, s23 | 798: vldr  s10, [r0] | 748: vldr  s11, [r0, #8] |
| 10e5c: ee4b7a08 vmla.f32 s15, s22, s16 | 79c: add  r0, r6, r4, lsl #2 | 74c: vsub.f32  s6, s2, s4 |
| 10e60: ee0a9a6b vmls.f32 s18, s20, s23 | 7a0: vldr  s9, [r0] | 750: vmul.f32  s1, s3, s8 |
| 10e64: e1570000 cmp r7, r0 | 7a4: add  r0, r7, r4, lsl #2 | 754: vmul.f32  s1, s1, s8 |
| 10e68: ece67a01 vstmia r6!, {s15} | 7a8: vldr  s12, [r0] | 758: vdiv.f32  s0, s1, s9 |
| 10e6c: edd57a00 vldr s15, [r5] | 7ac: vsub.f32  s7, s2, s4 | 75c: vsub.f32  s0, s11, s0 |
| 10e70: ee477a08 vmla.f32 s15, s14, s16 | 7b0: vmul.f32  s1, s3, s9 | 760: vmul.f32  s12, s6, s0 |
| 10e74: ece57a01 vstmia r5!, {s15} | 7b4: vmul.f32  s1, s1, s9 | 764: vsub.f32  s0, s13, s15 |
| 10e78: edd37a00 vldr s15, [r3] | 7b8: vdiv.f32  s0, s1, s10 | 768: vmul.f32  s10, s3, s0 |
| 10e7c: ee497a08 vmla.f32 s15, s18, s16 | 7bc: vsub.f32  s0, s12, s0 | 76c: vmul.f32  s1, s2, s12 |
| 10e80: ece47a01 vstmia r4!, {s15} | 7c0: vmul.f32  s13, s7, s0 | 770: vdiv.f32  s0, s1, s9 |
| 10e84: 0a000029 beq 10f30 ***EXIT LOOP*** | 7c4: vsub.f32  s0, s14, s16 | 774: vsqrt.f32  s14, s0 |
| 10e88: e1a0a002 mov sl, r2 | 7c8: vmul.f32  s11, s3, s0 | 778: mov  r0, #12 |
| 10e8c: e2822004 add r2, r2, #4 | 7cc: vmul.f32  s1, s2, s13 | 77c: mla  r0, r3, r0, r6 |
| 10e90: ed979a00 vldr s18, [r7] | 7d0: vdiv.f32  s0, s1, s10 | 780: vmla.f32  s16, s10, s9 |
| 10e94: e1a09007 mov r9, r7 | 7d4: vsqrt.f32  s15, s0 | 784: vstr  s16, [r0] |
| 10e98: e2877004 add r7, r7, #4 | 7d8: add  r0, r8, r3, lsl #2 | 788: vmul.f32  s0, s9, s14 |
| 10e9c: edd27a00 vldr s15, [r2] | 7dc: vmla.f32  s17, s11, s10 | 78c: vsub.f32  s0, s8, s0 |
| 10ea0: e1a08001 mov r8, r1 | 7e0: vstr  s17, [r0] | 790: vmla.f32  s17, s10, s0 |
| 10ea4: e2811004 add r1, r1, #4 | 7e4: add  r0, r9, r3, lsl #2 | 794: vstr  s17, [r0, #4] |
| 10ea8: edda9a00 vldr s19, [sl] | 7e8: vmul.f32  s0, s10, s15 | 798: vadd.f32  s5, s11, s12 |
| 10eac: ed977a00 vldr s14, [r7] | 7ec: vsub.f32  s0, s9, s0 | 79c: vmls.f32  s5, s8, s14 |
| 10eb0: ed516a01 vldr s13, [r1, #-4] | 7f0: vmla.f32  s18, s11, s0 | 7a0: vmla.f32  s18, s10, s5 |
| 10eb4: ee799aa7 vadd.f32 s19, s19, s15 | 7f4: vstr  s18, [r0] | 7a4: vstr  s18, [r0, #8] |
| 10eb8: eddb5a01 vldr s11, [fp, #4] | 7f8: add  r0, sl, r3, lsl #2 | 7a8: add  r3, r3, #1 |
| 10ebc: ee399a07 vadd.f32 s18, s18, s14 | 7fc: vadd.f32  s6, s12, s13 | 7ac: cmp  r3, r7 |
| 10ec0: edd17a00 vldr s15, [r1] | 800: vmls.f32  s6, s9, s15 | 7b0: blt  0x5f0 |
| 10ec4: ee699aa8 vmul.f32 s19, s19, s17 | 804: vmla.f32  s19, s11, s6 | |
| 10ec8: ee357aea vsub.f32 s14, s11, s21 | 808: vstr  s19, [r0] | |
| 10ecc: ee299a28 vmul.f32 s18, s18, s17 | 80c: add  r3, r3, #1 | |
| 10ed0: ee766aa7 vadd.f32 s13, s13, s15 | 810: ldr  r0, [fp, #32] | |
| 10ed4: ee296aa8 vmul.f32 s12, s19, s17 | 814: cmp  r3, r0 | |
| 10ed8: ee266a29 vmul.f32 s12, s12, s19 | 818: blt 0x640 | |
| 10edc: eec67a09 vdiv.f32 s15, s12, s18 | | |
| 10ee0: ee567aa8 vnmls.f32 s15, s13, s17 | | |
| 10ee4: ee677a87 vmul.f32 s15, s15, s14 | | |
| 10ee8: ee677aa5 vmul.f32 s15, s15, s11 | | |
| 10eec: ee870a89 vdiv.f32 s0, s15, s18 | | |
| 10ef0: eeb50a40 vcmp.f32 s0, #0.0 | | |
| 10ef4: eeb1cac0 vsqrt.f32 s24, s0 | | |
| 10ef8: eef1fa10 vmrs APSR_nzcv, fpscr | | |
| 10efc: 4a000013 bmi 10f50 | | |
| 10f00: ee898a89 vdiv.f32 s16, s19, s18 | | |
| 10f04: eeb58ac0 vcmpe.f32 s16, #0.0 | | |
| 10f08: eef1fa10 vmrs APSR_nzcv, fpscr | | |
| 10f0c: aaffff7c bge 10d04 | | |
| 10f10: e1a00001 mov r0, r1 | | |
| 10f14: e1a0c002 mov ip, r2 | | |
| 10f18: e1a03007 mov r3, r7 | | |
| 10f1c: eaffff7b b 10d10 | | |
| 10f20: e1a00001 mov r0, r1 | | |
| 10f24: e1a0e002 mov lr, r2 | | |
| 10f28: e1a0c007 mov ip, r7 | | |
| 10f2c: eaffff8f b 10d70 | | |
| 10f50: e58d2004 str r2, [sp, #4] | | |
| 10f54: e58d1008 str r1, [sp, #8] | | |
| 10f58: ebfffd37 bl 1043c <sqrtf@plt> | | |
| 10f5c: eddb7a01 vldr s15, [fp, #4] | | |
| 10f60: e59d2004 ldr r2, [sp, #4] | | |
| 10f64: e59d1008 ldr r1, [sp, #8] | | |
| 10f68: ee377aea vsub.f32 s14, s15, s21 | | |
| 10f6c: eaffffe3 b 10f00 | | |
| 10f70: e58d3004 str r3, [sp, #4] | | |
| 10f74: e58d2008 str r2, [sp, #8] | | |
| 10f78: e58d100c str r1, [sp, #12] | | |
| 10f7c: ebfffd2e bl 1043c <sqrtf@plt> | | |
| 10f80: e59d3004 ldr r3, [sp, #4] | | |
| 10f84: e59d2008 ldr r2, [sp, #8] | | |
| 10f88: e59d100c ldr r1, [sp, #12] | | |
| 10f8c: eaffffad b 10e48 | | |
| 10f90: e58d3004 str r3, [sp, #4] | | |
| 10f94: e58d2008 str r2, [sp, #8] | | |
| 10f98: e58d100c str r1, [sp, #12] | | |
| 10f9c: ebfffd26 bl 1043c <sqrtf@plt> | | |
| 10fa0: e59d3004 ldr r3, [sp, #4] | | |
| 10fa4: e59d2008 ldr r2, [sp, #8] | | |
| 10fa8: e59d100c ldr r1, [sp, #12] | | |
| 10fac: eaffff80 b 10db4 | | |

By the end of 2030, I expect to have automatic vectorization and/or parallelization working
in my HPC compiler.  The HPC extensions/restrictions make it "natural" to manage
parallel partitions, unlike the mess created by C standard semantics.

## Prerequisites
* This compiler project depends on several GNU/Linux behaviors, and it
  is necessary to have Arm/Linux installed in your build environment.
* Install [GNU Toolchain for the A-profile Architecture](https://developer.arm.com/tools-and-software/open-source-software/developer-tools/gnu-toolchain/gnu-a/downloads)
    - Select `arm-linux-none-gnueabihf` (AArch32 target with hard float)

## Test environment:
* Raspberry Pi 4B (SoC: bcm2711, ARMv8-A architecture)
* Raspbian GNU/Linux, kernel 5.10.17-v7l+, gcc 8.3.0 (armv7l userland)
* See the "Discussion" tab in this repo to run Squint on your ARM Chromebook.

The architecture support targets armv7hf with Linux ABI, verified on
Raspberry Pi 2/3/4 with GNU/Linux.

## Acknowledgements
AMaCC is based on the infrastructure of [c4](https://github.com/rswier/c4).
Please see the AMaCC repository [AMaCC](https://github.com/jserv/amacc) to
learn more about the AMaCC compiler.
