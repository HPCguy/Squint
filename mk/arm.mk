ARM_CC = gcc

# FIXME: check ld-linux.so as well
ARM_LD_LINUX_PATH := $(shell cd $(shell $(ARM_CC) --print-sysroot) 2>/dev/null && pwd)
ifeq ("$(ARM_LD_LINUX_PATH)","/") # packaged GNU toolchain
  ARM_LD_LINUX_PATH := $(shell dirname "$(shell which $(ARM_CC))")/..
  ARM_LD_LINUX_PATH := $(shell cd $(ARM_LD_LINUX_PATH) 2>/dev/null && pwd)
  ARM_LD_LINUX_PATH := $(ARM_LD_LINUX_PATH)/$(shell echo $(CROSS_COMPILE) | sed s'/.$$//')/libc
  ARM_LD_LINUX_PATH := $(shell cd $(ARM_LD_LINUX_PATH) 2>/dev/null && pwd)
  ifndef ARM_LD_LINUX_PATH
    ARM_LD_LINUX_PATH = /usr/$(shell echo $(CROSS_COMPILE) | sed s'/.$$//')
    ARM_LD_LINUX_PATH := $(shell cd $(ARM_LD_LINUX_PATH) 2>/dev/null && pwd)
  endif
endif
ifndef ARM_LD_LINUX_PATH
$(error "mc requires ld-linux.so")
endif

ARM_EXEC =
export ARM_EXEC
