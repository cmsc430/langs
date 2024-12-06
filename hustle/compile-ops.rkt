#lang racket
(provide compile-op0 compile-op1 compile-op2 pad-stack)
(require "ast.rkt")
(require "types.rkt")
(require "assert.rkt")
(require a86/ast)

(define rax 'rax)
(define ax  'ax)  ; pointer type tag
(define rbx 'rbx) ; heap
(define rdi 'rdi) ; arg
(define r8  'r8)  ; scratch in op2
(define r9  'r9)  ; scratch

(define r15 'r15) ; stack pad (non-volatile)
(define rsp 'rsp) ; stack

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
     (seq (Mov (Offset rbx 0) rax)    ; memory write
          (Mov rax rbx)               ; put box in rax
          (Shl rax 16)
          (Mov ax type-mutable-box)  ; tag as a mutable box
          (Add rbx 8))]
    ['box-immutable
     (seq (Mov (Offset rbx 0) rax)
          (Mov rax rbx)
          (Shl rax 16)
          (Mov ax type-immutable-box)
          (Add rbx 8))]
    ['unbox
     (seq (And ax #b11111101) ; delete the mut bit
          (Cmp ax type-box)
          (Jnz 'err)
          (Shr rax 16)
          (Mov rax (Offset rax 0)))]
    ['car
     (seq (Cmp ax type-cons)
          (Jnz 'err)
          (Shr rax 16)
          (Mov rax (Offset rax 8)))]
    ['cdr
     (seq (Cmp ax type-cons)
          (Jnz 'err)
          (Shr rax 16)
          (Mov rax (Offset rax 0)))]
    ['empty? (seq (Cmp rax (value->bits '())) if-equal)]
    ['cons?
     (seq (Mov r8 (value->bits #f))          
          (Cmp ax type-cons)
          (Mov rax (value->bits #t))
          (Cmovne rax r8))]          
    ['box?
     (seq (Mov r8 (value->bits #f))          
          (Cmp ax type-immutable-box)
          (Mov r9 (value->bits #t))
          (Cmp ax type-mutable-box)
          (Mov r9 (value->bits #t))
          (Mov rax r9)
          (Cmovne rax r8))]))


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
     (seq (Mov (Offset rbx 0) rax)
          (Pop rax)
          (Mov (Offset rbx 8) rax)
          (Mov rax rbx)
          (Shl rax 16)
          (Mov ax type-cons)
          (Add rbx 16))]
    ['eq?
     (seq (Pop r8)
          (Cmp rax r8)
          if-equal)]
    ['set-box!
     (seq (Pop r8)
          (Cmp 'r8w type-mutable-box)
          (Jnz 'err)
          (Sar r8 16)
          (Mov (Offset r8 0) rax)
          (Mov rax (value->bits (void))))]))

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

