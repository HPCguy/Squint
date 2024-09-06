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
| shock (float) | 39.192s | ***3.047s*** | 9.666s | 4.383s | 3.702s |
| nbody_arr (float) | 40.43s | ***3.295s*** | 13.04s | 4.019s | 3.894s |

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
| ***186++ instructions*** | ***119 instructions*** | ***113 instructions*** |
| ***??? inststructions/iter*** | ***119 instructions/iter*** | ***113 instructions/iter*** |
| 105e0: b  10770 | 640: add  r0, r5, r3, lsl #2 | 5f0: mov  r0, #12 |
| 105e4: mov  r0, r7 | 644: vldr  s1, [r0] | 5f4: mla  r0, r3, r0, r4 |
| 105e8: mov  lr, r9 | 648: vldr  s0, [r0, #4] | 5f8: vldr  s1, [r0] |
| 105ec: mov  ip, r8 | 64c: vadd.f32  s0, s1, s0 | 5fc: vldr  s0, [r0, #12] |
| 105f0: vldr  s22, [lr] | 650: vmul.f32  s10, s3, s0 | 600: vadd.f32  s0, s1, s0 |
| 105f4: vcvt.f64.f32  d7, s21 | 654: add  r0, r6, r3, lsl #2 | 604: vmul.f32  s9, s3, s0 |
| 105f8: vadd.f32  s24, s28, s21 | 658: vldr  s1, [r0] | 608: vldr  s1, [r0, #4] |
| 105fc: vldr  s23, [ip] | 65c: vldr  s0, [r0, #4] | 60c: vldr  s0, [r0, #16] |
| 10600: vldr  s3, [r0] | 660: vadd.f32  s0, s1, s0 | 610: vadd.f32  s0, s1, s0 |
| 10604: vcvt.f64.f32  d2, s22 | 664: vmul.f32  s9, s3, s0 | 614: vmul.f32  s8, s3, s0 |
| 10608: vmul.f64  d7, d7, d6 | 668: add  r0, r7, r3, lsl #2 | 618: vldr  s1, [r0, #8] |
| 1060c: vcmpe.f32  s24, #0.0 | 66c: vldr  s1, [r0] | 61c: vldr  s0, [r0, #20] |
| 10610: vcvt.f64.f32  d3, s23 | 670: vldr  s0, [r0, #4] | 620: vadd.f32  s0, s1, s0 |
| 10614: vcvt.f64.f32  d5, s3 | 674: vadd.f32  s0, s1, s0 | 624: vmul.f32  s11, s3, s0 |
| 10618: vmul.f64  d4, d2, d9 | 678: vmul.f32  s12, s3, s0 | 628: vsub.f32  s6, s2, s4 |
| 1061c: vcvt.f32.f64  s14, d7 | 67c: vsub.f32  s7, s2, s4 | 62c: vmul.f32  s1, s3, s8 |
| 10620: vmrs  APSR_nzcv, fpscr | 680: vmul.f32  s1, s3, s9 | 630: vmul.f32  s1, s1, s8 |
| 10624: vmul.f64  d4, d4, d2 | 684: vmul.f32  s1, s1, s9 | 634: vdiv.f32  s0, s1, s9 |
| 10628: vmul.f32  s23, s23, s14 | 688: vdiv.f32  s0, s1, s10 | 638: vsub.f32  s0, s11, s0 |
| 1062c: vmul.f32  s22, s22, s14 | 68c: vsub.f32  s0, s12, s0 | 63c: vmul.f32  s12, s6, s0 |
| 10630: vdiv.f64  d2, d4, d3 | 690: vmul.f32  s13, s7, s0 | 640: vmul.f32  s1, s2, s12 |
| 10634: vstr  s23, [r3] | 694: vmul.f32  s1, s2, s13 | 644: vdiv.f32  s0, s1, s9 |
| 10638: vstr  s22, [r2] | 698: vdiv.f32  s0, s1, s10 | 648: vsqrt.f32  s15, s0 |
| 1063c: vsub.f64  d5, d5, d2 | 69c: vsqrt.f32  s16, s0 | 64c: vdiv.f32  s13, s8, s9 |
| 10640: vcvt.f32.f64  s10, d5 | 6a0: vdiv.f32  s14, s9, s10 | 650: vcmpe.f32  s13, #0.0 |
| 10644: vsub.f32  s20, s3, s10 | 6a4: vcmpe.f32  s14, s5 | 654: vmrs  APSR_nzcv, fpscr |
| 10648: vmul.f32  s20, s20, s14 | 6a8: vmrs APSR_nzcv, fpscr | 658: mov  r0, r3 |
| 1064c: vstr  s20, [r1] | 6ac: mov  r0, r3 | 65c: bge  0x664 |
| 10650: blt  1081c | 6b0: bge 0x6b8 | 660: add  r0, r3, #1 |
| 10654: mov  r0, r7 | 6b4: add  r0, r3, #1 | 664: mov  r5, r0 |
| 10658: mov  lr, r9 | 6b8: mov  r4, r0 | 668: mov  r0, #12 |
| 1065c: mov  ip, r8 | 6bc: add  r0, r5, r4, lsl #2 | 66c: mla  r0, r5, r0, r4 |
| 10660: vldr  s29, [lr] | 6c0: vldr  s10, [r0] | 670: vldr  s9, [r0] |
| 10664: vmul.f32  s24, s24, s17 | 6c4: add  r0, r6, r4, lsl #2 | 674: vldr  s8, [r0, #4] |
| 10668: vldr  s27, [ip] | 6c8: vldr  s9, [r0] | 678: vldr  s11, [r0, #8] |
| 1066c: vldr  s26, [r0] | 6cc: add  r0, r7, r4, lsl #2 | 67c: vmul.f32  s1, s3, s8 |
| 10670: vcvt.f64.f32  d5, s29 | 6d0: vldr  s12, [r0] | 680: vmul.f32  s1, s1, s8 |
| 10674: vcvt.f64.f32  d3, s27 | 6d4: vmul.f32  s1, s3, s9 | 684: vdiv.f32  s0, s1, s9 |
| 10678: vcvt.f64.f32  d7, s26 | 6d8: vmul.f32  s1, s1, s9 | 688: vsub.f32  s12, s11, s0 |
| 1067c: vmul.f64  d4, d5, d9 | 6dc: vdiv.f32  s0, s1, s10 | 68c: vsub.f32  s0, s2, s4 |
| 10680: vmul.f64  d5, d4, d5 | 6e0: vsub.f32  s13, s12, s0 | 690: vmul.f32  s10, s13, s0 |
| 10684: vdiv.f64  d4, d5, d3 | 6e4: vsub.f32  s0, s2, s4 | 694: vmul.f32  s16, s10, s9 |
| 10688: vsub.f64  d7, d7, d4 | 6e8: vmul.f32  s11, s14, s0 | 698: vmul.f32  s17, s10, s8 |
| 1068c: vmul.f64  d7, d7, d6 | 6ec: vmul.f32  s17, s11, s10 | 69c: vsub.f32  s0, s11, s12 |
| 10690: vcvt.f32.f64  s14, d7 | 6f0: vmul.f32  s18, s11, s9 | 6a0: vmul.f32  s18, s10, s0 |
| 10694: vmul.f32  s15, s14, s25 | 6f4: vsub.f32  s0, s12, s13 | 6a4: vadd.f32  s1, s13, s15 |
| 10698: vdiv.f32  s0, s15, s27 | 6f8: vmul.f32  s19, s11, s0 | 6a8: vcmpe.f32  s1, #0.0 |
| 1069c: vcmp.f32  s0, #0.0 | 6fc: vadd.f32  s1, s14, s16 | 6ac: vmrs  APSR_nzcv, fpscr |
| 106a0: vsqrt.f32  s15, s0 | 700: vcmpe.f32  s1, s5 | 6b0: mov  r0, r3 |
| 106a4: vmrs  APSR_nzcv, fpscr | 704: vmrs APSR_nzcv, fpscr | 6b4: bge  0x6bc |
| 106a8: bmi  10b48 | 708: mov  r0, r3 | 6b8: add  r0, r3, #1 |
| 106ac: vadd.f32  s26, s26, s14 | 70c: bge 0x714 | 6bc: mov  r5, r0 |
| 106b0: vmov.f32  s14, s29 | 710: add  r0, r3, #1 | 6c0: mov  r0, #12 |
| 106b4: vsub.f32  s21, s21, s28 | 714: mov  r4, r0 | 6c4: mla  r0, r5, r0, r4 |
| 106b8: vmla.f32  s23, s27, s24 | 718: add  r0, r5, r4, lsl #2 | 6c8: vldr  s9, [r0] |
| 106bc: vmla.f32  s14, s27, s15 | 71c: vldr  s10, [r0] | 6cc: vldr  s8, [r0, #4] |
| 106c0: vmla.f32  s26, s29, s15 | 720: add  r0, r6, r4, lsl #2 | 6d0: vldr  s11, [r0, #8] |
| 106c4: vcmpe.f32  s21, #0.0 | 724: vldr  s9, [r0] | 6d4: vsub.f32  s6, s2, s4 |
| 106c8: vmla.f32  s22, s14, s24 | 728: add  r0, r7, r4, lsl #2 | 6d8: vmul.f32  s1, s3, s8 |
| 106cc: vmla.f32  s20, s26, s24 | 72c: vldr  s12, [r0] | 6dc: vmul.f32  s1, s1, s8 |
| 106d0: vstr  s23, [r3] | 730: vsub.f32  s7, s2, s4 | 6e0: vdiv.f32  s0, s1, s9 |
| 106d4: vmrs  APSR_nzcv, fpscr | 734: vmul.f32  s1, s3, s9 | 6e4: vsub.f32  s0, s11, s0 |
| 106d8: vstr  s22, [r2] | 738: vmul.f32  s1, s1, s9 | 6e8: vmul.f32  s12, s6, s0 |
| 106dc: vstr  s20, [r1] | 73c: vdiv.f32  s0, s1, s10 | 6ec: vadd.f32  s0, s13, s15 |
| 106e0: bge  106f0 | 740: vsub.f32  s0, s12, s0 | 6f0: vmul.f32  s10, s3, s0 |
| 106e4: mov  r7, r6 | 744: vmul.f32  s13, s7, s0 | 6f4: vmul.f32  s1, s2, s12 |
| 106e8: mov  r9, r5 | 748: vadd.f32  s0, s14, s16 | 6f8: vdiv.f32  s0, s1, s9 |
| 106ec: mov  r8, r4 | 74c: vmul.f32  s11, s3, s0 | 6fc: vsqrt.f32  s14, s0 |
| 106f0: vldr  s27, [r9] | 750: vmul.f32  s1, s2, s13 | 700: vmla.f32  s16, s10, s9 |
| 106f4: vmul.f32  s21, s21, s17 | 754: vdiv.f32  s0, s1, s10 | 704: vmul.f32  s0, s9, s14 |
| 106f8: vldr  s26, [r8] | 758: vsqrt.f32  s15, s0 | 708: vadd.f32  s0, s8, s0 |
| 106fc: vldr  s24, [r7] | 75c: vmla.f32  s17, s11, s10 | 70c: vmla.f32  s17, s10, s0 |
| 10700: vcvt.f64.f32  d5, s27 | 760: vmul.f32  s0, s10, s15 | 710: vadd.f32  s5, s11, s12 |
| 10704: vcvt.f64.f32  d3, s26 | 764: vadd.f32  s0, s9, s0 | 714: vmla.f32  s5, s8, s14 |
| 10708: vcvt.f64.f32  d7, s24 | 768: vmla.f32  s18, s11, s0 | 718: vmla.f32  s18, s10, s5 |
| 1070c: vmul.f64  d4, d5, d9 | 76c: vadd.f32  s6, s12, s13 | 71c: vsub.f32  s1, s13, s15 |
| 10710: vmul.f64  d5, d4, d5 | 770: vmla.f32  s6, s9, s15 | 720: vcmpe.f32  s1, #0.0 |
| 10714: vdiv.f64  d4, d5, d3 | 774: vmla.f32  s19, s11, s6 | 724: vmrs  APSR_nzcv, fpscr |
| 10718: vsub.f64  d7, d7, d4 | 778: vsub.f32  s1, s14, s16 | 728: mov  r0, r3 |
| 1071c: vmul.f64  d7, d7, d6 | 77c: vcmpe.f32  s1, s5 | 72c: bge  0x734 |
| 10720: vcvt.f32.f64  s14, d7 | 780: vmrs APSR_nzcv, fpscr | 730: add  r0, r3, #1 |
| 10724: vmul.f32  s15, s14, s25 | 784: mov  r0, r3 | 734: mov  r5, r0 |
| 10728: vdiv.f32  s0, s15, s26 | 788: bge 0x790 | 738: mov  r0, #12 |
| 1072c: vcmp.f32  s0, #0.0 | 78c: add  r0, r3, #1 | 73c: mla  r0, r5, r0, r4 |
| 10730: vsqrt.f32  s28, s0 | 790: mov  r4, r0 | 740: vldr  s9, [r0] |
| 10734: vmrs  APSR_nzcv, fpscr | 794: add  r0, r5, r4, lsl #2 | 744: vldr  s8, [r0, #4] |
| 10738: bmi  10b14 | 798: vldr  s10, [r0] | 748: vldr  s11, [r0, #8] |
| 1073c: vadd.f32  s24, s24, s14 | 79c: add  r0, r6, r4, lsl #2 | 74c: vsub.f32  s6, s2, s4 |
| 10740: vmov.f32  s15, s27 | 7a0: vldr  s9, [r0] | 750: vmul.f32  s1, s3, s8 |
| 10744: ldr  r0, [sp, #52]  ; 0x34 | 7a4: add  r0, r7, r4, lsl #2 | 754: vmul.f32  s1, s1, s8 |
| 10748: vmla.f32  s23, s26, s21 | 7a8: vldr  s12, [r0] | 758: vdiv.f32  s0, s1, s9 |
| 1074c: vmls.f32  s15, s26, s28 | 7ac: vsub.f32  s7, s2, s4 | 75c: vsub.f32  s0, s11, s0 |
| 10750: cmp  r0, r4 | 7b0: vmul.f32  s1, s3, s9 | 760: vmul.f32  s12, s6, s0 |
| 10754: vmls.f32  s24, s27, s28 | 7b4: vmul.f32  s1, s1, s9 | 764: vsub.f32  s0, s13, s15 |
| 10758: vmla.f32  s22, s15, s21 | 7b8: vdiv.f32  s0, s1, s10 | 768: vmul.f32  s10, s3, s0 |
| 1075c: vmla.f32  s20, s24, s21 | 7bc: vsub.f32  s0, s12, s0 | 76c: vmul.f32  s1, s2, s12 |
| 10760: vstmia  r3!, {s23} | 7c0: vmul.f32  s13, s7, s0 | 770: vdiv.f32  s0, s1, s9 |
| 10764: vstmia  r2!, {s22} | 7c4: vsub.f32  s0, s14, s16 | 774: vsqrt.f32  s14, s0 |
| 10768: vstmia  r1!, {s20} | 7c8: vmul.f32  s11, s3, s0 | 778: mov  r0, #12 |
| 1076c: beq  1085c ***EXIT LOOP*** | 7cc: vmul.f32  s1, s2, s13 | 77c: mla  r0, r3, r0, r6 |
| 10770: mov  r9, r5 | 7d0: vdiv.f32  s0, s1, s10 | 780: vmla.f32  s16, s10, s9 |
| 10774: add  r5, r5, #4 | 7d4: vsqrt.f32  s15, s0 | 784: vstr  s16, [r0] |
| 10778: vldr  s20, [r4] | 7d8: add  r0, r8, r3, lsl #2 | 788: vmul.f32  s0, s9, s14 |
| 1077c: mov  r8, r4 | 7dc: vmla.f32  s17, s11, s10 | 78c: vsub.f32  s0, s8, s0 |
| 10780: add  r4, r4, #4 | 7e0: vstr  s17, [r0] | 790: vmla.f32  s17, s10, s0 |
| 10784: vldr  s15, [r5] | 7e4: add  r0, r9, r3, lsl #2 | 794: vstr  s17, [r0, #4] |
| 10788: mov  r7, r6 | 7e8: vmul.f32  s0, s10, s15 | 798: vadd.f32  s5, s11, s12 |
| 1078c: add  r6, r6, #4 | 7ec: vsub.f32  s0, s9, s0 | 79c: vmls.f32  s5, s8, s14 |
| 10790: vldr  s22, [r9] | 7f0: vmla.f32  s18, s11, s0 | 7a0: vmla.f32  s18, s10, s5 |
| 10794: vldr  s11, [r4] | 7f4: vstr  s18, [r0] | 7a4: vstr  s18, [r0, #8] |
| 10798: vldr  s14, [r6, #-4] | 7f8: add  r0, sl, r3, lsl #2 | 7a8: add  r3, r3, #1 |
| 1079c: vadd.f32  s22, s22, s15 | 7fc: vadd.f32  s6, s12, s13 | 7ac: cmp  r3, r7 |
| 107a0: vldr  d6, [pc, #144] | 800: vmls.f32  s6, s9, s15 | 7b0: blt  0x5f0 |
| 107a4: vadd.f32  s20, s20, s11 | 804: vmla.f32  s19, s11, s6 | |
| 107a8: vldr  s15, [r6] | 808: vstr  s19, [r0] | |
| 107ac: vmul.f32  s22, s22, s17 | 80c: add  r3, r3, #1 | |
| 107b0: vsub.f64  d6, d15, d6 | 810: ldr  r0, [fp, #32] | |
| 107b4: vmul.f32  s20, s20, s17 | 814: cmp  r3, r0 | |
| 107b8: vadd.f32  s14, s14, s15 | 818: blt 0x640 | |
| 107bc: vcvt.f64.f32  d5, s22 | | |
| 107c0: vcvt.f64.f32  d3, s20 | | |
| 107c4: vmul.f32  s14, s14, s17 | | |
| 107c8: vmul.f64  d4, d5, d9 | | |
| 107cc: vcvt.f64.f32  d7, s14 | | |
| 107d0: vmul.f64  d4, d4, d5 | | |
| 107d4: vdiv.f64  d5, d4, d3 | | |
| 107d8: vsub.f64  d7, d7, d5 | | |
| 107dc: vmul.f64  d7, d7, d6 | | |
| 107e0: vcvt.f32.f64  s14, d7 | | |
| 107e4: vmul.f32  s14, s14, s25 | | |
| 107e8: vdiv.f32  s0, s14, s20 | | |
| 107ec: vcmp.f32  s0, #0.0 | | |
| 107f0: vsqrt.f32  s28, s0 | | |
| 107f4: vmrs  APSR_nzcv, fpscr | | |
| 107f8: bmi  10ae8 | | |
| 107fc: vdiv.f32  s21, s22, s20 | | |
| 10800: vcmpe.f32  s21, #0.0 | | |
| 10804: vmrs  APSR_nzcv, fpscr | | |
| 10808: bge  105e4 | | |
| 1080c: mov  r0, r6 | | |
| 10810: mov  lr, r5 | | |
| 10814: mov  ip, r4 | | |
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
