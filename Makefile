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
EXEC = $(BIN) $(BIN)-native $(PEEP) $(BIN)-so

include mk/arm.mk
include mk/common.mk

## Build mc and squint
all: $(EXEC)
$(BIN): $(BIN).c
	$(VECHO) "  CC+LD\t\t$@\n"
	$(Q)$(ARM_CC) $(CFLAGS) -o $@ $< -g -ldl

$(BIN)-so: $(BIN).c $(PEEP).c
	$(VECHO) "  CC+LD\t\t$@\n"
	$(Q)$(ARM_CC) -DSQUINT_SO $(CFLAGS) -c -fpic $(PEEP).c
	$(Q)$(ARM_CC) -shared -o lib$(PEEP).so $(PEEP).o
	$(Q)$(ARM_CC) -DSQUINT_SO -g $(CFLAGS) $(CURR_DIR)/lib$(PEEP).so -o $@ $< -ldl

$(BIN)-native: $(BIN).c
	$(VECHO) "  CC+LD\t\t$@\n"
	$(Q)$(CC) $(CFLAGS) -o $@ $< \
	    -Wno-pointer-to-int-cast -Wno-int-to-pointer-cast -Wno-format \
	    -ldl

$(PEEP): $(PEEP).c
	$(VECHO) "  CC+LD\t\t$@\n"
	$(Q)$(ARM_CC) $(CFLAGS) -o $@ $< -g


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
	@echo "Type 'make show_asm' to create assembly listing in ASM directory"

bench: $(EXEC)
	$(Q)$(ARM_EXEC) $(OBJ_DIR)/$(BIN)-opt $(OP) -o $(OBJ_DIR)/lulesh-opt $(TEST_DIR)/extra/lulesh.c
	$(Q) scripts/peep $(OBJ_DIR)/lulesh-opt -e
	$(Q)$(ARM_EXEC) time $(OBJ_DIR)/lulesh-opt
	$(VECHO) "\n\n"
	$(Q)$(ARM_EXEC) $(OBJ_DIR)/$(BIN)-opt $(OP) -o $(OBJ_DIR)/nbody_arr-opt $(TEST_DIR)/extra/nbody_arr.c
	$(Q) scripts/peep $(OBJ_DIR)/nbody_arr-opt -e
	$(Q)$(ARM_EXEC) time $(OBJ_DIR)/nbody_arr-opt

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
	$(VECHO) "[*** verify $< <ELF> *******]\n"
	$(Q)$(ARM_EXEC) ./$(BIN) -o $(OBJ_DIR)/$(notdir $(basename $<)) $< $(REDIR)
	$(Q)$(ARM_EXEC) $(OBJ_DIR)/$(notdir $(basename $<)) 2 $(REDIR)
	$(VECHO) "[*** verify $< <ELF-self> **]\n"
	$(Q)$(ARM_EXEC) ./$(OBJ_DIR)/$(BIN) $< 2 $(REDIR)
	$(VECHO) "[*** verify $< <JIT-opt> *******]\n"
	$(Q)$(ARM_EXEC) ./$(BIN)-so $(OP) $< 2 $(REDIR)
	$(VECHO) "[*** verify $< <ELF-so-opt> *******]\n"
	$(Q)$(ARM_EXEC) ./$(BIN)-so $(OP) -o $(OBJ_DIR)/$(notdir $(basename $<))-opt $< $(REDIR)
	$(Q)$(ARM_EXEC) $(OBJ_DIR)/$(notdir $(basename $<))-opt 2 $(REDIR)
	$(VECHO) "[*** verify $< <ELF-opt> *******]\n"
	$(Q)$(ARM_EXEC) $(OBJ_DIR)/$(BIN)-opt $(OP) -o $(OBJ_DIR)/$(notdir $(basename $<))-opt $< $(REDIR)
	$(Q) scripts/peep $(OBJ_DIR)/$(notdir $(basename $<))-opt
	$(Q)$(ARM_EXEC) $(OBJ_DIR)/$(notdir $(basename $<))-opt 2 $(REDIR)
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
