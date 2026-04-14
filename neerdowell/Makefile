ifeq ($(shell uname), Darwin)
  LANGS_CC ?= arch -x86_64 clang
  LANGS_AS ?= arch -x86_64 clang -c
else
  LANGS_CC ?= clang
  LANGS_AS ?= clang -c
endif

RACKET ?= racket

RUNTIME_DIR := runtime/standalone
RUNTIME := $(RUNTIME_DIR)/runtime.o

# Example source extension for this language.
SRC_EXT := rkt

default:
	@echo "example: make foo.run"

# Build the runtime bundles if needed.
$(RUNTIME):
	$(MAKE) -C $(RUNTIME_DIR)

# Compile source program to assembly.
%.s: %.$(SRC_EXT)
	cat $< | $(RACKET) -t compiler/compile-stdin.rkt -m > $@

# Assemble to object.
%.o: %.s
	$(LANGS_AS) -o $@ $<

# Link standalone executable.
%.run: %.o $(RUNTIME)
	$(LANGS_CC) -o $@ $^

clean:
	@$(RM) *.o *.s *.run ||:
	@$(MAKE) -C $(RUNTIME_DIR) clean
