ifeq ($(shell uname), Darwin)
  LANGS_CC ?= arch -x86_64 clang
  LANGS_AS ?= arch -x86_64 clang -c
else
  LANGS_CC ?= clang
  LANGS_AS ?= clang -c
endif

objs = \
	main.o \
	print.o \
	values.o

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
