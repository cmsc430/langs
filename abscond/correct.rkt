#lang racket
(provide check-compiler)
(require rackunit)
(require "interp.rkt")
(require "exec.rkt")

;; Expr -> Void
(define (check-compiler e)
  (check-equal? (interp e)
                (exec e)))

