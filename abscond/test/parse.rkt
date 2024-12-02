#lang racket
(require "../parse.rkt")
(require "../ast.rkt")
(require rackunit)

(define (p e)
  e)

(begin ; Abscond
  (check-equal? (parse 42) (p (Lit 42)))
  (check-equal? (parse -1) (p (Lit -1))))
(begin ; ¬ Abscond
  (check-exn exn:fail? (λ () (parse #t)))
  (check-exn exn:fail? (λ () (parse '(add1 8)))))

