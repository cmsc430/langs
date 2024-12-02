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
(begin ; Dupe
 (check-equal? (parse '(if (zero? 1) 2 3))
               (p (If (Prim1 'zero? (Lit 1)) (Lit 2) (Lit 3))))
 (check-equal? (parse '(if #t 2 3))
               (p (If (Lit #t) (Lit 2) (Lit 3)))))
(begin ; Dodger
  (check-equal? (parse #\a) (p (Lit #\a)))
  (check-equal? (parse '(char->integer #\a))
                (p (Prim1 'char->integer (Lit #\a)))))
(begin ; Evildoer
  (check-equal? (parse 'eof) (p (Eof)))
  (check-equal? (parse '(void)) (p (Prim0 'void)))
  (check-equal? (parse '(read-byte)) (p (Prim0 'read-byte))))
(begin ; Fraud
  (check-equal? (parse 'x) (p (Var 'x)))
  (check-exn exn:fail? (λ () (parse-closed 'x)))
  (check-equal? (parse '(+ 1 2))
                (p (Prim2 '+ (Lit 1) (Lit 2))))
  (check-equal? (parse '(let ((x 1)) x))
                (p (Let 'x (Lit 1) (Var 'x))))
  (check-equal? (parse-closed '(let ((x 1)) x))
                (p (Let 'x (Lit 1) (Var 'x))))
  (check-equal? (parse 'add1) (p (Var 'add1)))
  (check-exn exn:fail? (λ () (parse-closed 'add1)))
  (check-equal? (parse '(let ((let 1)) let))
                (p (Let 'let (Lit 1) (Var 'let))))
  (check-equal? (parse '(let ((if 1)) if))
                (p (Let 'if (Lit 1) (Var 'if)))))
(begin ; Hustle
  (check-equal? (parse ''()) (p (Lit '())))
  (check-equal? (parse '(box 1)) (p (Prim1 'box (Lit 1))))
  (check-equal? (parse '(cons 1 2)) (p (Prim2 'cons (Lit 1) (Lit 2)))))
(begin ; Hoax
  (check-equal? (parse "asdf") (p (Lit "asdf")))
  (check-equal? (parse '(make-string 10 #\a))
                (p (Prim2 'make-string (Lit 10) (Lit #\a)))))

