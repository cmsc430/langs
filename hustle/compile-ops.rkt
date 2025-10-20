#lang racket
(provide compile-op0 compile-op1 compile-op2 pad-stack)
(require "ast.rkt")
(require "types.rkt")
(require "assert.rkt")
(require a86/ast a86/registers)

;; Op0 -> Asm
(define (compile-op0 p)
  (match p
    ['void      (seq (Mov rax (value->bits (void))))]
    ['read-byte (seq pad-stack (Call 'read_byte) unpad-stack)]
    ['peek-byte (seq pad-stack (Call 'peek_byte) unpad-stack)]))

;; Op1 -> Asm
(define (compile-op1 p)
  (match p
    ['add1
     (seq (assert-integer rax)
          (Add rax (value->bits 1)))]
    ['sub1
     (seq (assert-integer rax)
          (Sub rax (value->bits 1)))]
    ['zero?
     (seq (assert-integer rax)
          (Cmp rax 0)
          if-equal)]
    ['char?
     (seq (And rax mask-char)
          (Cmp rax type-char)
          if-equal)]
    ['char->integer
     (seq (assert-char rax)
          (Sar rax char-shift)
          (Sal rax int-shift))]
    ['integer->char
     (seq (assert-codepoint rax)
          (Sar rax int-shift)
          (Sal rax char-shift)
          (Xor rax type-char))]
    ['eof-object?
     (seq (Cmp rax (value->bits eof))
          if-equal)]
    ['write-byte
     (seq (assert-byte rax)
          pad-stack
          (Mov rdi rax)
          (Call 'write_byte)
          unpad-stack)]
    ['box
     (seq (Mov (Mem rbx) rax) ; memory write
          (Mov rax rbx)            ; put box in rax
          (Xor rax type-box)       ; tag as a box
          (Add rbx 8))]
    ['unbox
     (seq (assert-box rax)
          (Mov rax (Mem rax (- type-box))))]
    ['car
     (seq (assert-cons rax)
          (Mov rax (Mem rax (- 0 type-cons))))]
    ['cdr
     (seq (assert-cons rax)
          (Mov rax (Mem rax (- 8 type-cons))))]

    ['empty? (seq (Cmp rax (value->bits '())) if-equal)]
    ['cons? (type-pred ptr-mask type-cons)]
    ['box?  (type-pred ptr-mask type-box)]))


;; Op2 -> Asm
(define (compile-op2 p)
  (match p
    ['+
     (seq (Pop r8)
          (assert-integer r8)
          (assert-integer rax)
          (Add rax r8))]
    ['-
     (seq (Pop r8)
          (assert-integer r8)
          (assert-integer rax)
          (Sub r8 rax)
          (Mov rax r8))]
    ['<
     (seq (Pop r8)
          (assert-integer r8)
          (assert-integer rax)
          (Cmp r8 rax)
          if-lt)]
    ['=
     (seq (Pop r8)
          (assert-integer r8)
          (assert-integer rax)
          (Cmp r8 rax)
          if-equal)]
    ['cons
     (seq (Mov (Mem rbx 8) rax)
          (Pop rax)
          (Mov (Mem rbx 0) rax)
          (Mov rax rbx)
          (Xor rax type-cons)
          (Add rbx 16))]
    ['eq?
     (seq (Pop r8)
          (Cmp rax r8)
          if-equal)]))

(define (type-pred mask type)
  (seq (And rax mask)
       (Cmp rax type)
       if-equal))

;; Asm
;; set rax to #t or #f if comparison flag is equal
(define if-equal
  (seq (Mov rax (value->bits #f))
       (Mov r9  (value->bits #t))
       (Cmove rax r9)))

;; Asm
;; set rax to #t or #f if comparison flag is less than
(define if-lt
  (seq (Mov rax (value->bits #f))
       (Mov r9  (value->bits #t))
       (Cmovl rax r9)))


;; Asm
;; Dynamically pad the stack to be aligned for a call
(define pad-stack
  (seq (Mov r15 rsp)
       (And r15 #b1000)
       (Sub rsp r15)))

;; Asm
;; Undo the stack alignment after a call
(define unpad-stack
  (seq (Add rsp r15)))

