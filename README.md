# Squint: A peephole optimizer for stack VM compilers

## Introduction
Short summary: This repo contains a highly functional, but not quite
complete, C compiler (mc.c) that supports JIT execution, ELF executable
generation, and peephole optimization.  mc.c is a follow on to the AMaCC
compiler.  See the AMaCC documentation referenced below for more information.

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
* squint optimizer -- 2900 SLOC

The original AMaCC compiler is based on the phenomenal work of the 
team at https://github.com/jserv/amacc , and I strongly suggest
you visit that site to see the history of AMaCC compiler development
as stored in that repository.  It shows how small changes can be
added to a base complier, step-by-step,  to create a truly marvelous
educational compiler. The README there expands upon this README and
is where you will learn the most about the AMaCC compiler that was
used as a starting point for this work.

## Performance

| Benchmark |  AMaCC .text size | Mc+Squint .text | Gcc -O3 .text | Notes |
| --- | --- | --- | --- | --- |
| bezier.c | 3672 | 1172 | ***768*** | recursive |
| duff.c | 3068 | 564 | ***412*** | unusual |
| maze.c | 6640 | 2632 | ***1752*** | misc |
| shock.c | 8732 | ***2032*** | 3388 | floating point |
| mc.c | 123264 | 61240 | ***34932*** | full compiler |

| Benchmark | AMaCC compile time | Mc+Squint time | Gcc -O3 time |
| --- | --- | --- | --- |
| mc.c | ***0.140s*** | 0.352s | 3.462s |

| Benchmark | AMaCC runtime | Mc+Squint | Gcc | Gcc -O1 | Gcc -03 |
| --- | --- | --- | --- | --- | --- |
| sieve (int) | 3.676s |  ***0.936s*** | 1.642s | 0.942s | 0.962s |
| shock (float) | 39.192s | ***3.628s*** | 9.666s | 4.383s | 3.702s |
| fib 42 (int) | 10.546s | 4.595s | 6.209s | 4.504s | ***3.553s*** |

Note: shock run with 8192 elements, 4096 timesteps, no output. Best of 20 runs.


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
