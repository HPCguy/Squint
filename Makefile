SHELL := bash
CFLAGS = -g -Wall -Wno-misleading-indentation -Wno-incompatible-pointer-types \
         -Wno-maybe-uninitialized
OP = -Op
CURR_DIR=$(abspath $(shell pwd))
OBJ_DIR = ELF
ASM_DIR = ASM
IR_DIR = IR
TEST_DIR = tests
TEST_SRC = $(wildcard $(TEST_DIR)/*.c)
TEST_OBJ = $(TEST_SRC:.c=.o)

BIN = mc
PEEP = squint
EXEC = $(BIN) $(BIN)-native $(PEEP) $(BIN)-so # $(BIN)o

include mk/arm.mk
include mk/common.mk

## Build mc and squint
all: $(EXEC)
$(BIN): $(BIN).c
	$(VECHO) "  CC+LD\t\t$@\n"
	$(Q)$(ARM_CC) $(CFLAGS) -o $@ $< -g -ldl

$(BIN)-so: $(BIN).c $(PEEP).c
	$(VECHO) "  CC+LD\t\t$@\n"
	$(Q)if [ "$(shell uname -m)" != "aarch64" ]; then
	$(Q)$(ARM_CC) -DSQUINT_SO=1 $(CFLAGS) -c -fpic $(PEEP).c
	$(Q)$(ARM_CC) -shared -o lib$(PEEP).so $(PEEP).o
	$(Q)$(ARM_CC) -DSQUINT_SO=1 -g $(CFLAGS) $(CURR_DIR)/lib$(PEEP).so -o $@ $< -ldl
	else
	$(Q)$(ARM_CC) -DSQUINT_SO=1 -g $(CFLAGS) -o $@ $< -ldl
	fi

$(BIN)-native: $(BIN).c
	$(VECHO) "  CC+LD\t\t$@\n"
	$(Q)$(CC) $(CFLAGS) -o $@ $< \
	    -Wno-pointer-to-int-cast -Wno-int-to-pointer-cast -Wno-format \
	    -ldl

$(PEEP): $(BIN) $(PEEP).c
	$(VECHO) "  CC+LD\t\t$@\n"
	# Build an optimizer with ICACHE packing disabled
	$(Q)$(ARM_EXEC) ./$(BIN) -Op -DNO_PACK_ICACHE=1 -o $(PEEP).tmp $(PEEP).c
	$(Q)$(ARM_EXEC) cp $(PEEP).tmp $(PEEP)
	$(Q)$(ARM_EXEC) scripts/peep $(PEEP).tmp
	$(Q)$(ARM_EXEC) cp $(PEEP).tmp $(PEEP)
	# Build an optimizer that is not *itself* ICACHE packed
	# but that will generate ICACHE packed executables.
	$(Q)$(ARM_EXEC) ./mc -Op -o $(PEEP).tmp $(PEEP).c
	$(Q)$(ARM_EXEC) scripts/peep $(PEEP).tmp
	$(Q)$(ARM_EXEC) cp $(PEEP).tmp $(PEEP)
	$(Q)$(ARM_EXEC) rm $(PEEP).tmp

# All the floating point tests fail with this executable.
# Needs to be re-enabled after that issue is fixed.
# $(BIN)o: $(BIN) $(PEEP)
# 	$(VECHO) "  CC+LD\t\t$@\n"
# 	# Build an optimizer with ICACHE packing disabled
# 	$(Q)$(ARM_EXEC) cat $(PEEP).c $(BIN).c > $(BIN)o.c
# 	$(Q)$(ARM_EXEC) ./$(BIN) -Op -DSQUINT_SO=1 -o $(BIN)o $(BIN)o.c
# 	$(Q)$(ARM_EXEC) scripts/peep $(BIN)o
# 	# Build an optimizer that is not *itself* ICACHE packed
# 	# but that will generate ICACHE packed executables.
# 	# When this is enabled, comment out lines at end of $(PEEP) target
# 	$(Q)$(ARM_EXEC) ./$(BIN) -Op -o $(PEEP).tmp $(PEEP).c
# 	$(Q)$(ARM_EXEC) scripts/peep $(PEEP).tmp
# 	$(Q)$(ARM_EXEC) cp $(PEEP).tmp $(PEEP)
# 	$(Q)$(ARM_EXEC) rm $(PEEP).tmp
# 	$(Q)$(ARM_EXEC) touch ./$(BIN)o


## Run tests and show message
check: $(EXEC) $(TEST_OBJ)
	$(VECHO) "[ C to IR translation          ]"
	$(Q)./$(BIN)-native -s tests/arginc.c | diff tests/arginc.list - \
	    && $(call pass)
	$(VECHO) "[ JIT compilation + execution  ]"
	$(Q)if [ "$(shell $(ARM_EXEC) ./$(BIN) tests/hello.c)" = "hello, world" ]; then \
	$(call pass); \
	fi
	$(VECHO) "[ ELF generation               ]"
	$(Q)$(ARM_EXEC) ./$(BIN) -o $(OBJ_DIR)/hello tests/hello.c
	$(Q)if [ "$(shell $(ARM_EXEC) $(OBJ_DIR)/hello)" = "hello, world" ]; then \
	$(call pass); \
	fi
	$(VECHO) "[ nested/self compilation      ]"
	$(Q)if [ "$(shell $(ARM_EXEC) ./$(BIN) $(BIN).c tests/hello.c)" = "hello, world" ]; then \
	$(call pass); \
	fi
	@echo
	@echo "Type 'make show_asm' to create assembly listing in ASM directory"

bench: $(EXEC) $(OBJ_DIR)/$(BIN)-opt
	$(Q)$(ARM_EXEC) $(OBJ_DIR)/$(BIN)-opt $(OP) -o $(OBJ_DIR)/lulesh-opt $(TEST_DIR)/extra/lulesh.c
	$(Q) scripts/peep $(OBJ_DIR)/lulesh-opt -e
	$(Q) time $(ARM_EXEC) $(OBJ_DIR)/lulesh-opt
	$(Q)$(ARM_EXEC) $(OBJ_DIR)/$(BIN)-opt $(OP) -o $(OBJ_DIR)/lulesh_p-opt $(TEST_DIR)/extra/lulesh_p.c
	$(Q) scripts/peep $(OBJ_DIR)/lulesh_p-opt -e
	$(Q) time $(ARM_EXEC) $(OBJ_DIR)/lulesh_p-opt
	$(Q)$(ARM_EXEC) $(OBJ_DIR)/$(BIN)-opt $(OP) -o $(OBJ_DIR)/lulesh_s-opt $(TEST_DIR)/extra/lulesh_s.c
	$(Q) scripts/peep $(OBJ_DIR)/lulesh_s-opt -e
	$(Q) time $(ARM_EXEC) $(OBJ_DIR)/lulesh_s-opt
	$(VECHO) "\n\n"
	$(Q)$(ARM_EXEC) $(OBJ_DIR)/$(BIN)-opt $(OP) -o $(OBJ_DIR)/nbody_arr-opt $(TEST_DIR)/extra/nbody_arr.c
	$(Q) scripts/peep $(OBJ_DIR)/nbody_arr-opt -e
	$(Q) time $(ARM_EXEC) $(OBJ_DIR)/nbody_arr-opt

$(OBJ_DIR)/$(BIN): $(BIN)
	$(VECHO) "  SelfCC\t$@\n"
	$(Q)$(ARM_EXEC) ./$^ -o $@ $(BIN).c

$(OBJ_DIR)/$(BIN)-opt: $(BIN) $(PEEP)
	$(VECHO) "  SelfCC\t$@\n"
	$(Q)$(ARM_EXEC) ./$< $(OP) -o $@ $(BIN).c
	$(Q) scripts/peep $@

.ONESHELL:
SHELL_HACK := $(shell mkdir -p $(IR_DIR) $(OBJ_DIR) $(ASM_DIR))
$(TEST_DIR)/%.o: $(TEST_DIR)/%.c $(BIN) $(OBJ_DIR)/$(BIN) $(OBJ_DIR)/$(BIN)-opt
	$(VECHO) "[*** verify $< <IR> *******]\n"
	$(Q)$(ARM_EXEC) ./$(BIN) -si $< > $(IR_DIR)/$(notdir $(basename $<))
	$(VECHO) "[*** verify $< <JIT> *******]\n"
	$(Q)$(ARM_EXEC) ./$(BIN) $< 2 $(REDIR)
	$(VECHO) "[*** verify $< <JIT-opt> *******]\n"
	$(Q)$(ARM_EXEC) ./$(BIN)-so $< 2 $(REDIR)
	$(VECHO) "[*** verify $< <ELF> *******]\n"
	$(Q)$(ARM_EXEC) ./$(BIN) -o $(OBJ_DIR)/$(notdir $(basename $<)) $< $(REDIR)
	$(Q)$(ARM_EXEC) $(OBJ_DIR)/$(notdir $(basename $<)) 2 $(REDIR)
	$(VECHO) "[*** verify $< <ELF-self> **]\n"
	$(Q)$(ARM_EXEC) ./$(OBJ_DIR)/$(BIN) $< 2 $(REDIR)
	$(VECHO) "[*** verify $< <ELF-so-opt> *******]\n"
	$(Q)$(ARM_EXEC) ./$(BIN)-so -o $(OBJ_DIR)/$(notdir $(basename $<))-opt $< $(REDIR)
	$(Q)$(ARM_EXEC) $(OBJ_DIR)/$(notdir $(basename $<))-opt 2 $(REDIR)
	$(VECHO) "[*** verify $< <ELF-opt> *******]\n"
	$(Q)$(ARM_EXEC) $(OBJ_DIR)/$(BIN)-opt $(OP) -o $(OBJ_DIR)/$(notdir $(basename $<))-opt $< $(REDIR)
	$(Q) scripts/peep $(OBJ_DIR)/$(notdir $(basename $<))-opt
	$(Q)$(ARM_EXEC) $(OBJ_DIR)/$(notdir $(basename $<))-opt 2 $(REDIR)
	# Re-enable this when mco FP bug is fixed.
	# $(VECHO) "[*** verify $< <mco> *******]\n"
	# $(Q)$(ARM_EXEC) ./$(BIN)o -o $(OBJ_DIR)/$(notdir $(basename $<))-opt $< $(REDIR)
	# $(Q)$(ARM_EXEC) $(OBJ_DIR)/$(notdir $(basename $<))-opt 2 $(REDIR)
	$(Q)$(call pass,$<)

show_asm:
	cd $(OBJ_DIR); for name in * ; do echo $${name};\
    ../scripts/disasm $${name} | egrep -v nop | tail --lines=+28 | \
    awk '/e92d4[80]00/{print "--func--"}1' > ../$(ASM_DIR)/$${name}.s; done

## Print available build targets
help:
	@cat $(MAKEFILE_LIST) | \
	 awk '/^##.*$$/{l1=$$0;getline;l2=(l1 "##" $$0); print l2 $$0}' | awk -F"##" '{split($$3,t,":");printf "\033[36m%-11s\033[0m %s\n",t[1],$$2}'

## Dump assembly from source file. Usage: "make dump-ir FILE=tests/hello.c"
dump-ir: $(BIN)
	@$(ARM_EXEC) $(BIN) -s $(FILE)

## Remove all generated files
clean:
	$(RM) $(EXEC) $(OBJ_DIR)/* $(PEEP).o lib$(PEEP).so $(ASM_DIR)/* $(IR_DIR)/*
