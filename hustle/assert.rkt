#lang racket
(provide assert-integer assert-char assert-byte assert-codepoint
         assert-box assert-cons)
(require a86/ast)
(require "types.rkt")

(define r9 'r9)
(define r8 'r8)

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

(define assert-box
  (assert-type #xFF type-box))
(define assert-cons
  (assert-type #xFF type-cons))

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

