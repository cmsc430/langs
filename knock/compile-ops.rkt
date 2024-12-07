#lang racket
(provide compile-op0 compile-op1 compile-op2 compile-op3 pad-stack)
(require "ast.rkt")
(require "types.rkt")
(require "assert.rkt")
(require a86/ast)

(define rax 'rax)
(define rdi 'rdi)
(define rbx 'rbx)
(define r8 'r8)
(define r9 'r9)
(define r10 'r10)
(define r15 'r15)
(define rsp 'rsp)

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
     (seq (Mov (Offset rbx 0) rax)
          (Mov rax rbx)
          (address->type rax type-mutable-box)
          (Add rbx 8))]
    ['box-immutable
     (seq (Mov (Offset rbx 0) rax)
          (Mov rax rbx)
          (address->type rax type-immutable-box)
          (Add rbx 8))]
    ['unbox
     (seq (box->address rax)
          (Mov rax (Offset rax 0)))]
    ['car
     (seq (cons->address rax)
          (Mov rax (Offset rax 8)))]
    ['cdr
     (seq (cons->address rax)
          (Mov rax (Offset rax 0)))]
    ['empty? (seq (Cmp rax (value->bits '())) if-equal)]
    ['cons?
     (seq (Mov r8 (value->bits #f))
          (Cmp (reg-16-bit rax) type-cons)
          (Mov rax (value->bits #t))
          (Cmovne rax r8))]
    ['box?
     (seq (Mov r8 (value->bits #f))
          (And (reg-16-bit rax) zero-mut)
          (Cmp (reg-16-bit rax) type-box)
          (Mov rax (value->bits #t))
          (Cmovne rax r8))]
    ['vector?
     (seq (Mov r8 (value->bits #f))
          (And (reg-16-bit rax) zero-mut)
          (Cmp (reg-16-bit rax) type-vector)
          (Mov rax (value->bits #t))
          (Cmovne rax r8))]
    ['string?
     (seq (Mov r8 (value->bits #f))
          (And (reg-16-bit rax) zero-mut)
          (Cmp (reg-16-bit rax) type-string)
          (Mov rax (value->bits #t))
          (Cmovne rax r8))]
    ['vector-length
     (seq (vector->address rax)
          (Mov rax (Offset rax 0)))]
    ['string-length
     (seq (string->address rax)
          (Mov rax (Offset rax 0)))]))


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
          (address->type rax type-cons)
          (Add rbx 16))]
    ['eq?
     (seq (Pop r8)
          (Cmp rax r8)
          if-equal)]
    ['set-box!
     (seq (Pop r8)
          (mutable-box->address r8)
          (Mov (Offset r8 0) rax)
          (Mov rax (value->bits (void))))]
    ['make-vector ;; size value
     (let ((loop (gensym))
           (done (gensym))
           (empty (gensym)))
       (seq (Pop r8)
            (assert-natural r8)
            (Cmp r8 0) ; special case empty string
            (Je empty)
            (Mov r9 rbx)
            (address->type r9 type-mutable-vector)
            (Mov (Offset rbx 0) r8)
            (Add rbx 8)
            (Label loop)
            (Mov (Offset rbx 0) rax)
            (Add rbx 8)
            (Sub r8 (value->bits 1))
            (Cmp r8 0)
            (Jne loop)
            (Mov rax r9)
            (Jmp done)
            (Label empty)
            (Lea rax 'the_empty_sequence)
            (address->type rax type-immutable-vector)
            (Label done)))]
    ['vector-ref ; vector index
     (seq (Pop r8)
          (vector->address r8)
          (assert-integer rax)
          (Cmp rax 0)
          (Jl 'err)
          (Mov r9 (Offset r8 0))  ; r9 = len
          (Sub r9 (value->bits 1))
          (Cmp r9 rax)
          (Jl 'err)
          (Sal rax 1)
          (Add r8 rax)
          (Mov rax (Offset r8 8)))]
    ['make-string
     (let ((loop (gensym))
           (done (gensym))
           (empty (gensym)))
       (seq (Pop r8)
            (assert-natural r8)
            (assert-char rax)
            (Cmp r8 0) ; special case empty string
            (Je empty)
            (Mov r9 rbx)
            (address->type r9 type-mutable-string)
            (Mov (Offset rbx 0) r8)
            (Add rbx 8)
            (Label loop)
            (Mov (Offset rbx 0) rax)
            (Add rbx 8)
            (Sub r8 (value->bits 1))
            (Cmp r8 0)
            (Jne loop)
            (Mov rax r9)
            (Jmp done)
            (Label empty)
            (Lea rax 'the_empty_sequence)
            (address->type rax type-immutable-string)
            (Label done)))]
    ['string-ref ; string index
     (seq (Pop r8)
          (string->address r8)
          (assert-integer rax)
          (Cmp rax 0)
          (Jl 'err)
          (Mov r9 (Offset r8 0))  ; r9 = len
          (Sub r9 (value->bits 1))
          (Cmp r9 rax)
          (Jl 'err)
          (Sal rax 1)
          (Add r8 rax)
          (Mov rax (Offset r8 8)))]))

;; Op3 -> Asm
(define (compile-op3 p)
  (match p
    ['vector-set! ; vector index value
     (seq (Pop r10) ; index
          (Pop r8)  ; value
          (mutable-vector->address r8)
          (assert-integer r10)
          (Cmp r10 0)
          (Jl 'err)
          (Mov r9 (Offset r8 0))  ; r9 = len
          (Sub r9 (value->bits 1))
          (Cmp r9 r10)
          (Jl 'err)
          (Sal r10 1)
          (Add r8 r10)
          (Mov (Offset r8 8) rax)
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

