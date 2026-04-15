#lang racket
(provide interp)
(require "../syntax/ast.rkt")

;; Expr -> Integer
(define (interp e)
  (match e
    [(Lit i) i]))

