#lang racket
(provide check-compiler)
(require rackunit)
(require "interpreter/interp-io.rkt")
(require "executor/exec.rkt")
(require "compiler/compile.rkt")
;; Expr String -> Void
(define (check-compiler e i)
  (let ((r (with-handlers ([exn:fail? identity])
             (interp/io e i))))
    (unless (exn? r)
      (check-equal? r (exec/io e i)))))

