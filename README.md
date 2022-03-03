# Squint: A peephole optimizer for stack VM compilers

## Introduction
Short summary: This repo contains a highly functional, but not quite
complete, C compiler (mc.c) that supports JIT execution, ELF executable
generation, and peephole optimization.  mc.c is a follow on to the AMaCC
compiler.  See the AMaCC documentation referenced below for more information.

This compiler supports the following features beyond AMaCC:

* Float data types (AMaCC is an integer based compiler).

* The Squint peephole optimizer that ***roughly halves the number of executable
instructions in compiled code***.  The tests/sieve.c
benchmark provides an optimization example, and runs roughly
***3.5x faster after peephole optimization***.

Source code size:
* mc -- 2500 SLOC
* squint -- 1500 SLOC

The original AMaCC compiler is based on the phenomenal work of the 
team at https://github.com/jserv/amacc , and I strongly suggest
you visit that site to see the history of AMaCC compiler development
as stored in that repository.  It shows how small changes can be
added to a base complier, step-by-step,  to create a truly marvelous
educational compiler. The README there expands upon this README and
is where you will learn the most about the AMaCC compiler that was
used as a starting point for this work.

## Example
The following command uses the mc compiler to JIT compile an optimized verison
of the mc compiler, using an external optimizer linked as a shared object library,
and then that optimized JIT complier is used to JIT compile an optimized version of
Sieve of Eratosthenes (again using a shared object optimizer), and then the
sieve benchmark is JIT executed.  The time shown is the time it takes to do
everything in this paragraph.
```
$ make mc-so      # use gcc to build an enhanced version of the mc compiler
  CC+LD		mc-so
$ time ./mc-so -DSQUINT_SO -Op mc.c -Op tests/sieve.c

real	0m2.614s
user	0m2.582s
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
