#lang racket
(provide check-compiler)
(require rackunit)
(require "interp-io.rkt")
(require "exec-io.rkt")
;; ClosedExpr String -> Void
(define (check-compiler e i)
  (check-equal? (interp/io e i)
                (exec/io e i)))

