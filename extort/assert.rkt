#lang racket
(provide assert-integer assert-char assert-byte assert-codepoint)
(require a86/ast)
(require "types.rkt")

(define r9 'r9)

;; Register -> Asm
(define (assert-integer r)
  (seq (Mov r9 r)
       (And r9 mask-int)
       (Cmp r9 type-int)
       (Jne 'err)))

;; Register -> Asm
(define (assert-char r)
  (seq (Mov r9 r)
       (And r9 mask-char)
       (Cmp r9 type-char)
       (Jne 'err)))

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

