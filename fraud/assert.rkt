#lang racket
(provide assert-integer assert-char assert-byte assert-codepoint)
(require a86/ast)
(require "types.rkt")

;; Register -> Asm
(define (assert-integer r)
  (seq (Push r)
       (And r mask-int)
       (Cmp r type-int)
       (Pop r)
       (Jne 'err)))

;; Register -> Asm
(define (assert-char r)
  (seq (Push r)
       (And r mask-char)
       (Cmp r type-char)
       (Pop r)
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

