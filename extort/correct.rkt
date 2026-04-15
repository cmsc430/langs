#lang racket
(provide check-compiler)
(require rackunit)
(require "interpreter/interp-io.rkt")
(require "executor/run.rkt")
(require "compiler/compile.rkt")
;; Expr String -> Void
(define (check-compiler e i)
  (check-equal? (interp/io e i)
                (run/io (compile e) i)))

