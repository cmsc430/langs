#lang racket
(require "../parse.rkt")
(require "../ast.rkt")
(require rackunit)

(define (p e)
  e)

(begin ; Abscond
  (check-equal? (parse 42) (p (Lit 42)))
  (check-equal? (parse -1) (p (Lit -1))))
(check-equal? (parse '(add1 42)) (p (Prim1 'add1 (Lit 42))))
(check-equal? (parse '(if (zero? 1) 2 3))
              (p (IfZero (Lit 1) (Lit 2) (Lit 3))))

