#lang racket
(provide check-compiler)
(require rackunit)
(require "interp-io.rkt")
(require "exec.rkt")
;; Expr String -> Void
(define (check-compiler e i)
  (check-equal? (interp/io e i)
                (exec/io e i)))

