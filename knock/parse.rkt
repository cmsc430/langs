#lang racket
(provide parse parse-closed parse-e parse-define parse-pattern)
(require "ast.rkt")

;; [Listof S-Expr] -> Prog
(define (parse . ss)
  (match (parse-prog ss (parse-defn-names ss) '() '() '())
    [(list _ _ p) p]))

;; [Listof S-Expr] -> ClosedProg
(define (parse-closed . ss)
  (match (parse-prog ss (parse-defn-names ss) '() '() '())
    [(list '() '() p) p]
    [(list ys gs p) (error "undefined identifiers" (append ys gs))]))

;; S-Expr -> Expr
;; Parse a (potentially open) expression
(define (parse-e s)
  (match (parse-e/acc s '() '() '() '())
    [(list _ _ e) e]))

;; S-Expr -> Expr
;; Parse a (potentially open) definition
(define (parse-define s)
  (match (parse-define/acc s '() '() '() '())
    [(list _ _ d) d]))

;; S-Expr -> Pat
;; Parse a (potentially open) pattern
(define (parse-pattern s)
  (match (parse-match-pattern/acc s '() '() '() '())
    [(list _ _ _ p) p]))

;; S-Expr -> r:[Listof Id]
;;   where: (distinct? r)
;; Extracts defined function names from given program-like s-expr
;; Does not fully parse definition
;; Example:
;;   (parse-defn-names '((define (f x) x) (define (g y) y) 1) -> '(f g)
(define (parse-defn-names ss)
  (define (rec ss fs)
    (match ss
      [(list s) fs]
      [(cons (cons 'define sd) sr)
       (match (parse-defn-name sd)
         [f (if (memq f fs)
                (error "duplicate definition" f)
                (rec sr (cons f fs)))])]
      [_ (error "parse error")]))
  (rec ss '()))

(define (parse-defn-name s)
  (match s
    [(cons (cons (? symbol? f) _) _) f]
    [_ (error "parse error")]))

;; S-Expr [Listof Id] [Listof Id] [Listof Id] [Listof Id] -> (list [Listof Id] [Listof Id] Prog)
;;   s: program shaped s-expr to be parsed
;;   fs: defined function names
;;   xs: bound variables
;;   ys: free variables
;;   gs: undefined function names
;; returns list of free variables, undefined function names, and parse of program
(define (parse-prog s fs xs ys gs)
  (match s
    [(list s)
     (match (parse-e/acc s fs xs ys gs)
       [(list ys gs e)
        (list ys gs (Prog '() e))])]
    [(cons s ss)
     (match (parse-define/acc s fs xs ys gs)
       [(list ys gs (and d (Defn f _ _)))
        (match (parse-prog ss (cons f fs) xs ys gs)
          [(list ys gs (Prog ds e))
           (list ys gs (Prog (cons d ds) e))])])]))

;; S-Expr [Listof Id] [Listof Id] [Listof Id] [Listof Id] -> (list [Listof Id] [Listof Id] Defn)
;;   s: definition shaped s-expr to be parsed
;;   fs: defined function names
;;   xs: bound variables
;;   ys: free variables
;;   gs: undefined function names
;; returns list of free variables, undefined function names, and parse of definition
;; returns list of free variables and parse of definition
(define (parse-define/acc s fs xs ys gs)
  (match s
    [(cons 'define sr)
     (match sr
       [(list (cons (? symbol? g) (and (list (? symbol? zs) ...) (? distinct?)))  s)
        (match (parse-e/acc s (cons g fs) (append zs xs) ys gs)
          [(list ys gs e)
           (list ys gs (Defn g zs e))])]
       [_ (error "parse error")])]
    [_ (error "parse error")]))

;; S-Expr [Listof Id] [Listof Id] [Listof Id] [Listof Id] -> (list [Listof Id] [Listof Id] Expr)
;;   s: expression shaped s-expr to be parsed
;;   fs: defined function names
;;   xs: bound variables
;;   ys: free variables
;;   gs: undefined function names
;; returns list of free variables, undefined function names, and parse of expression
(define (parse-e/acc s fs xs ys gs)
  (define (rec s xs ys gs)
    (define ns (append fs xs))
    (match s
      [(and 'eof (? (not-in ns)))
       (list ys gs (Eof))]
      [(? datum?)
       (list ys gs (Lit s))]
      [(list 'quote (list))
       (list ys gs (Lit '()))]
      [(? symbol? (? (not-in fs)))
       (if (memq s xs)
           (list ys gs (Var s))
           (list (cons s ys) gs (Var s)))]
      [(list-rest (? symbol? (? (not-in ns) k)) sr)
       (match k
         ['let
          (match sr
            [(list (list (list (? symbol? x) s1)) s2)
             (match (rec s1 xs ys gs)
               [(list ys gs e1)
                (match (rec s2 (cons x xs) ys gs)
                  [(list ys gs e2)
                   (list ys gs (Let x e1 e2))])])]
            [_ (error "let: bad syntax" s)])]
         ['match
           (match sr
             [(cons s sr)
              (match (rec s xs ys gs)
                [(list ys gs e)
                 (match (parse-match-clauses/acc sr fs xs ys gs)
                   [(list ys gs ps es)
                    (list ys gs (Match e ps es))])])]
             [_ (error "match: bad syntax" s)])]
         [_
          (match (parse-es/acc sr fs xs ys gs)
            [(list ys gs es)
             (match (cons k es)
               [(list (? op0? o))
                (list ys gs (Prim0 o))]
               [(list (? op1? o) e1)
                (list ys gs (Prim1 o e1))]
               [(list (? op2? o) e1 e2)
                (list ys gs (Prim2 o e1 e2))]
               [(list (? op3? o) e1 e2 e3)
                (list ys gs (Prim3 o e1 e2 e3))]
               [(list 'begin e1 e2)
                (list ys gs (Begin e1 e2))]
               [(list 'if e1 e2 e3)
                (list ys gs (If e1 e2 e3))]
               [(list-rest g es)
                (list ys (cons g gs) (App g es))])])])]
      [(list-rest (? symbol? g) sr)
       (match (parse-es/acc sr fs xs ys gs)
         [(list ys s es)
          (list ys (if (memq g fs) gs (cons g gs)) (App g es))])]
      [_
       (error "parse error" s)]))
  (rec s xs ys gs))

;; S-Expr [Listof Id] [Listof Id] [Listof Id] [Listof Id] -> (list [Listof Id] [Listof Id] [Listof Expr])
;;   s: list of expressions shaped s-expr to be parsed
;;   fs: defined function names
;;   xs: bound variables
;;   ys: free variables
;;   gs: undefined function names
;; returns list of free variables, undefined function names, and list of parsed expressions
(define (parse-es/acc s fs xs ys gs)
  (match s
    ['() (list ys gs '())]
    [(cons s ss)
     (match (parse-e/acc s fs xs ys gs)
       [(list ys gs e)
        (match (parse-es/acc ss fs xs ys gs)
          [(list ys gs es)
           (list ys gs (cons e es))])])]
    [_ (error "parse error")]))

;; S-Expr [Listof Id] [Listof Id] [Listof Id] [Listof Id] -> (list [Listof Id] [Listof Id] [Listof Expr])
;;   s: list of match clauses shaped s-expr to be parsed
;;   fs: defined function names
;;   xs: bound variables
;;   ys: free variables
;;   gs: undefined function names
;; returns list of free variables, undefined function names, and list of parsed clause patterns and clause expressions
(define (parse-match-clauses/acc sr fs xs ys gs)
  (match sr
    ['() (list ys gs '() '())]
    [(cons (list sp se) sr)
     (match (parse-match-pattern/acc sp fs xs ys gs)
       [(list ys xs gs p)
        (match (parse-e/acc se fs xs ys gs)
          [(list ys gs e)
           (match (parse-match-clauses/acc sr fs xs ys gs)
             [(list ys gs ps es)
              (list ys gs (cons p ps) (cons e es))])])])]))

;; S-Expr [Listof Id] [Listof Id] [Listof Id] [Listof Id] -> (list [Listof Id] [Listof Id] [Listof Id] Pat)
;;   s: list of patterns shaped s-expr to be parsed
;;   fs: defined function names
;;   xs: bound variables
;;   ys: free variables
;;   gs: undefined function names
;; returns list of free variables, bound variables, undefined function names, and parse of pattern
(define (parse-match-pattern/acc s fs xs ys gs)
  (define (rec p xs ys gs)
    (match p
      [(? datum?)  (list ys xs gs (Lit p))]
      ['_          (list ys xs gs (Var '_))]
      [(? symbol?) (list ys (cons p xs) gs (Var p))]
      [(list 'quote '())
       (list ys xs gs (Lit '()))]
      [(list 'box s)
       (match (rec s xs ys gs)
         [(list ys xs gs p)
          (list ys xs gs (Box p))])]
      [(list 'cons s1 s2)
       (match (rec s1 xs ys gs)
         [(list ys xs gs p1)
          (match (rec s2 xs ys gs)
            [(list ys xs gs p2)
             (list ys xs gs (Cons p1 p2))])])]
      [(list 'and s1 s2)
       (match (rec s1 xs ys gs)
         [(list ys xs gs p1)
          (match (rec s2 xs ys gs)
            [(list ys xs gs p2)
             (list ys xs gs (Conj p1 p2))])])]
      [_ (error "parse pattern error")]))
  (rec s xs ys gs))

;; [Listof Any] -> Boolean
(define (distinct? xs)
  (not (check-duplicates xs)))

;; xs:[Listof Any] -> p:(x:Any -> Boolean)
;; Produce a predicate p for things not in xs
(define (not-in xs)
  (λ (x) (not (memq x xs))))
(define (in m)
  (λ (x) (memq x m)))

;; Any -> Boolean
(define (datum? x)
  (or (exact-integer? x)
      (boolean? x)
      (char? x)
      (string? x)))

;; Any -> Boolean
(define (op0? x)
  (memq x '(read-byte peek-byte void)))

(define (op1? x)
  (memq x '(add1 sub1 zero? char? integer->char char->integer
                 write-byte eof-object?
                 box box-immutable unbox empty? cons? box? car cdr
                 vector? vector-length string? string-length)))

(define (op2? x)
  (memq x '(+ - < = eq? cons set-box!
              make-vector vector-ref make-string string-ref)))

(define (op3? x)
  (memq x '(vector-set!)))

