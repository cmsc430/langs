#lang racket
(provide check-compiler)
(require rackunit)
(require "interpreter/interp.rkt")
(require "compiler/compile.rkt")
(require a86/interp)

;; Expr -> Void
(define (check-compiler e)
  (check-equal? (interp e)
                (asm-interp (compile e))))

