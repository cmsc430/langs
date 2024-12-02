#lang racket
(provide parse)
(require "ast.rkt")

;; S-Expr -> Expr
(define (parse s)
  (match s
    [(? datum?) (Lit s)]
    [(list-rest (? symbol? k) sr)
     (match k
       [(? op1? o)
        (match sr
          [(list s1)
           (Prim1 o (parse s1))]
          [_ (error "op1: bad syntax" s)])]
       ['if
        (match sr
          [(list s1 s2 s3)
           (If (parse s1) (parse s2) (parse s3))]
          [_ (error "if: bad syntax" s)])]
       [_ (error "parse error" s)])]
    [_ (error "parse error" s)]))


;; Any -> Boolean
(define (datum? x)
  (or (exact-integer? x)
      (boolean? x)
      (char? x)))

(define (op1? x)
  (memq x '(add1 sub1 zero? char? integer->char char->integer)))

