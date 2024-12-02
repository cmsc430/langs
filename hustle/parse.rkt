#lang racket
(provide parse parse-closed)
(require "ast.rkt")

;; S-Expr -> Expr
(define (parse s)
  (match (parse/acc s '() '())
    [(list _ e) e]))

;; S-Expr -> ClosedExpr
(define (parse-closed s)
  (match (parse/acc s '() '())
    [(list '() e) e]
    [(list fvs e) (error "unbound identifiers" fvs)]))

;; S-Expr [Listof Id] [Listof Id] -> (list [Listof Id] Expr)
;; Parse s into expr and list of free variables
;; assuming bvs are bound, fvs are free
(define (parse/acc s bvs fvs)
  (define (rec s bvs fvs)
    (match s
      [(and 'eof (? (not-in bvs)))
       (list fvs (Eof))]
      [(? datum?)
       (list fvs (Lit s))]
      [(list (and 'quote (? (not-in bvs))) (list))
       (list fvs (Lit '()))]
      [(? symbol?)
       (list (if (memq s bvs) fvs (cons s fvs)) (Var s))]
      [(list-rest (? symbol? (? (not-in bvs) k)) sr)
       (match k
         ['let
          (match sr
            [(list (list (list (? symbol? x) s1)) s2)
             (match (parse/acc s1 bvs fvs)
               [(list fvs e1)
                (match (parse/acc s2 (cons x bvs) fvs)
                  [(list fvs e2)
                   (list fvs (Let x e1 e2))])])]
            [_ (error "let: bad syntax" s)])]
         [_
          (match (parse-es/acc sr bvs fvs)
            [(list fvs es)
             (list fvs
                   (match (cons k es)
                     [(list (? op0? o)) (Prim0 o)]
                     [(list (? op1? o) e1) (Prim1 o e1)]
                     [(list (? op2? o) e1 e2) (Prim2 o e1 e2)]
                     [(list 'begin e1 e2) (Begin e1 e2)]
                     [(list 'if e1 e2 e3) (If e1 e2 e3)]
                     [_ (error "bad syntax" s)]))])])]
      [_ (error "parse error" s)]))
  (rec s bvs fvs))

;; S-Expr [Listof Id] [Listof Id] -> (list [Listof Id] [Listof Expr])
(define (parse-es/acc s bvs fvs)
  (match s
    ['() (list fvs '())]
    [(cons s ss)
     (match (parse/acc s bvs fvs)
       [(list fvs e)
        (match (parse-es/acc ss bvs fvs)
          [(list fvs es)
           (list fvs (cons e es))])])]
    [_ (error "parse error")]))

;; [Listof Any] -> (Any -> Boolean)
(define (not-in m)
  (λ (x) (not (memq x m))))


;; Any -> Boolean
(define (datum? x)
  (or (exact-integer? x)
      (boolean? x)
      (char? x)))

;; Any -> Boolean
(define (op0? x)
  (memq x '(read-byte peek-byte void)))

(define (op1? x)
  (memq x '(add1 sub1 zero? char? integer->char char->integer
                 write-byte eof-object?
                 box unbox empty? cons? box? car cdr)))

(define (op2? x)
  (memq x '(+ - < = eq? cons)))

