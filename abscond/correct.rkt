#lang racket
(provide check-compiler)
(require rackunit)
(require "interpreter/interp.rkt")
(require "executor/run.rkt")
(require "compiler/compile.rkt")

;; Expr -> Void
(define (check-compiler e)
  (check-equal? (interp e)
                (run (compile e))))

