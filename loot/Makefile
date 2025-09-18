ifeq ($(shell uname), Darwin)
  LANGS_CC ?= arch -x86_64 gcc
  LANGS_AS ?= nasm -g -f macho64 --gprefix _
else
  LANGS_CC ?= gcc
  LANGS_AS ?= nasm -g -f elf64
endif

objs = \
	main.o \
	print.o \
	values.o \
	io.o

default: runtime.o

runtime.o: $(objs)
	ld -r $(objs) -o runtime.o

%.run: %.o runtime.o
	$(LANGS_CC) runtime.o $< -o $@

.c.o:
	$(LANGS_CC) -fPIC -c -g -o $@ $<

.s.o:
	$(LANGS_AS) -o $@ $<

%.s: %.rkt
	cat $< | racket -t compile-stdin.rkt -m > $@

clean:
	@$(RM) *.o *.s *.run ||:
	@echo "$(shell basename $(shell pwd)): cleaned!"

%.test: %.run %.rkt
	@test "$(shell ./$(<))" = "$(shell racket $(word 2,$^))"
