#lang racket
(provide check-compiler)
(require rackunit)
(require "interp.rkt")
(require "exec.rkt")

;; Expr -> Void
(define (check-compiler e)
  (let ((r (with-handlers ([exn:fail? identity])
             (interp e))))
    (unless (exn? r)
      (check-equal? r (exec e)))))

