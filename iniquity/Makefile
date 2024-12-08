UNAME := $(shell uname)

ifeq ($(UNAME), Darwin)
  format=macho64
  CC=arch -x86_64 gcc
  LD=ld -macos_version_min 14.0
else
  format=elf64
  CC=gcc
  LD=ld
endif

objs = \
	main.o \
	print.o \
	values.o \
	io.o

runtime.o: $(objs)
	$(LD) -r $(objs) -o runtime.o

%.run: %.o runtime.o
	$(CC) runtime.o $< -o $@

.c.o:
	$(CC) -fPIC -c -g -o $@ $<

.s.o:
	nasm -g -f $(format) -o $@ $<

%.s: %.rkt
	cat $< | racket -t compile-stdin.rkt -m > $@

clean:
	@$(RM) *.o *.s *.run ||:
	@echo "$(shell basename $(shell pwd)): cleaned!"

%.test: %.run %.rkt
	@test "$(shell ./$(<))" = "$(shell racket $(word 2,$^))"
