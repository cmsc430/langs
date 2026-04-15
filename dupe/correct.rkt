#lang racket
(provide check-compiler)
(require rackunit)
(require "interpreter/interp.rkt")
(require "executor/run.rkt")
(require "compiler/compile.rkt")

;; Expr -> Void
(define (check-compiler e)
  (let ((r (with-handlers ([exn:fail? identity])
             (interp e))))
    (unless (exn? r)
      (check-equal? r (run (compile e))))))

