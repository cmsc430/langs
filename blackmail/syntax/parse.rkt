#lang racket
(provide parse)
(require "ast.rkt")

;; S-Expr -> Expr
(define (parse s)
  (match s
    [(? exact-integer?) (Lit s)]
    [(list-rest (? symbol? k) sr)
     (match k
       [(? op1? o)
        (match sr
          [(list s1)
           (Prim1 o (parse s1))]
          [_ (error "op1: bad syntax" s)])]
       [_ (error "parse error" s)])]
    [_ (error "parse error" s)]))

(define (op1? x)
  (memq x '(add1 sub1)))

