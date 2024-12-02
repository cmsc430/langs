#lang racket
(provide check-compiler)
(require rackunit)
(require "interp-io.rkt")
(require "exec-io.rkt")
;; Expr String -> Void
(define (check-compiler e i)
  (let ((r (with-handlers ([exn:fail? identity])
             (interp/io e i))))
    (unless (exn? r)
      (check-equal? r (exec/io e i)))))

