ifeq ($(shell uname), Darwin)
  LANGS_CC ?= arch -x86_64 clang
  LANGS_AS ?= arch -x86_64 clang -c
else
  LANGS_CC ?= clang
  LANGS_AS ?= clang -c
endif

CFLAGS   += -fPIC -g

OBJS = \
	main.o \
	values.o \
	print.o \
	io.o \
	symbol.o \
	error.o

default: runtime.o

runtime.o: $(OBJS)
	ld -r $(OBJS) -o $@

%.o: %.c
	$(LANGS_CC) $(CFLAGS) -c -o $@ $<

clean:
	@$(RM) *.o runtime.o ||:
	@echo "$(shell basename $(shell pwd)): cleaned!"
