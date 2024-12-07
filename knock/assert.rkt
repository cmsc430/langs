#lang racket
(provide assert-integer assert-char assert-byte assert-codepoint
         cons->address box->address address->type
         mutable-box->address
         vector->address string->address
         mutable-vector->address mutable-string->address
         assert-natural)
(require a86/ast)
(require "types.rkt")

(define r9 'r9)

(define (cons->address r)
  (seq (Cmp (reg-16-bit r) type-cons)
       (Jne 'err)
       (Sar r 16)))

(define (box->address r)
  (seq (And (reg-16-bit r) zero-mut)
       (Cmp (reg-16-bit r) type-box)
       (Jne 'err)
       (Sar r 16)))

(define (mutable-box->address r)
  (seq (Cmp (reg-16-bit r) type-mutable-box)
       (Jne 'err)
       (Sar r 16)))

(define (vector->address r)
  (seq (And (reg-16-bit r) zero-mut)
       (Cmp (reg-16-bit r) type-vector)
       (Jne 'err)
       (Sar r 16)))

(define (mutable-vector->address r)
  (seq (Cmp (reg-16-bit r) type-mutable-vector)
       (Jne 'err)
       (Sar r 16)))

(define (mutable-string->address r)
  (seq (Cmp (reg-16-bit r) type-mutable-string)
       (Jne 'err)
       (Sar r 16)))

(define (string->address r)
  (seq (And (reg-16-bit r) zero-mut)
       (Cmp (reg-16-bit r) type-string)
       (Jne 'err)
       (Sar r 16)))

(define (address->type r t)
  (seq (Shl r 16)
       (Mov (reg-16-bit r) t)))

(define (assert-type mask type)
  (λ (arg)
    (seq (Mov r9 arg)
         (And r9 mask)
         (Cmp r9 type)
         (Jne 'err))))

;; Register -> Asm


(define assert-integer
  (assert-type mask-int type-int))

;; Register -> Asm

(define assert-char
  (assert-type mask-char type-char))

;; Register -> Asm
(define (assert-codepoint r)
  (let ((ok (gensym)))
    (seq (assert-integer r)
         (Cmp r (value->bits 0))
         (Jl 'err)
         (Cmp r (value->bits 1114111))
         (Jg 'err)
         (Cmp r (value->bits 55295))
         (Jl ok)
         (Cmp r (value->bits 57344))
         (Jg ok)
         (Jmp 'err)
         (Label ok))))

;; Register -> Asm
(define (assert-byte r)
  (seq (assert-integer r)
       (Cmp r (value->bits 0))
       (Jl 'err)
       (Cmp r (value->bits 255))
       (Jg 'err)))

;; Register -> Asm
(define (assert-natural r)
  (seq (assert-integer r)
       (Cmp r (value->bits 0))
       (Jl 'err)))

