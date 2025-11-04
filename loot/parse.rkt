#lang racket
(provide parse parse-closed parse-e parse-define parse-pattern)
(require "ast.rkt")

;; [Listof S-Expr] -> Prog
(define (parse . ss)
  (match (parse-prog ss (parse-defn-names ss) '())
    [(list _ p) p]))

;; [Listof S-Expr] -> ClosedProg
(define (parse-closed . ss)
  (match (parse-prog ss (parse-defn-names ss) '())
    [(list '() p) p]
    [(list ys p) (error "undefined identifiers" ys)]))

;; S-Expr -> Expr
;; Parse a (potentially open) expression
(define (parse-e s)
  (match (parse-e/acc s '() '())
    [(list _ e) e]))

;; S-Expr -> Expr
;; Parse a (potentially open) definition
(define (parse-define s)
  (match (parse-define/acc s '() '())
    [(list _ d) d]))

;; S-Expr -> Pat
;; Parse a (potentially open) pattern
(define (parse-pattern s)
  (match (parse-match-pattern/acc s '() '())
    [(list _ _ p) p]))

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
      [(cons (cons (? (not-in fs) 'define) sd) sr)
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
;; S-Expr [Listof Id] [Listof Id] -> (list [Listof Id] Prog)
;;   s: program shaped s-expr to be parsed
;;   xs: bound variables
;;   ys: free variables
;; returns list of free variables and parse of program
(define (parse-prog s xs ys)
  (match s
    [(list s)
     (match (parse-e/acc s xs ys)
       [(list ys e)
        (list ys (Prog '() e))])]
    [(cons s ss)
     (match (parse-define/acc s xs ys)
       [(list ys (and d (Defn f _ _)))
        (match (parse-prog ss xs ys)
          [(list ys (Prog ds e))
           (list ys (Prog (cons d ds) e))])])]))
;; S-Expr [Listof Id] [Listof Id] [Listof Id] [Listof Id] -> (list [Listof Id] Defn)
;;   s: definition shaped s-expr to be parsed
;;   xs: bound variables
;;   ys: free variables
;; returns list of free variables and parse of definition
(define (parse-define/acc s xs ys)
  (match s
    [(cons 'define sr)
     (match sr
       [(list (cons (? symbol? g) (and (list (? symbol? zs) ...) (? distinct?)))  s)
        (match (parse-e/acc s (cons g (append zs xs)) ys)
          [(list ys e)
           (list ys (Defn g zs e))])]
       [_ (error "parse error")])]
    [_ (error "parse error")]))
;; S-Expr [Listof Id] [Listof Id] [Listof Id] [Listof Id] -> (list [Listof Id] Expr)
;;   s: expression shaped s-expr to be parsed
;;   xs: bound variables
;;   ys: free variables
;; returns list of free variables and parse of expression
(define (parse-e/acc s xs ys)
  (define (rec s xs ys)
    (define ns xs)
    (match s
      [(and 'eof (? (not-in ns)))
       (list ys (Eof))]
      [(? datum?)
       (list ys (Lit s))]
      [(list 'quote (list))
       (list ys (Lit '()))]
      [(? symbol? f)
       (if (memq s xs)
           (list ys (Var s))
           (list (cons s ys) (Var s)))]
      [(list-rest (? symbol? (? (not-in ns) k)) sr)
       (match k
         ['let
          (match sr
            [(list (list (list (? symbol? x) s1)) s2)
             (match (rec s1 xs ys)
               [(list ys e1)
                (match (rec s2 (cons x xs) ys)
                  [(list ys e2)
                   (list ys (Let x e1 e2))])])]
            [_ (error "let: bad syntax" s)])]
         ['match
          (match sr
            [(cons s sr)
             (match (rec s xs ys)
               [(list ys e)
                (match (parse-match-clauses/acc sr xs ys)
                  [(list ys ps es)
                   (list ys (Match e ps es))])])]
            [_ (error "match: bad syntax" s)])]

         [(or 'λ 'lambda)
          (match sr
            [(list (and (list (? symbol? zs) ...) (? distinct?)) s)
             (match (rec s (append zs xs) ys)
               [(list ys e)
                (list ys (Lam (gensym 'lambda) zs e))])]
            [_ (error "lambda: bad syntax" s)])]
         [_
          (match (parse-es/acc sr xs ys)
            [(list ys es)
             (match (cons k es)
               [(list (? op0? o))
                (list ys (Prim0 o))]
               [(list (? op1? o) e1)
                (list ys (Prim1 o e1))]
               [(list (? op2? o) e1 e2)
                (list ys (Prim2 o e1 e2))]
               [(list (? op3? o) e1 e2 e3)
                (list ys (Prim3 o e1 e2 e3))]
               [(list 'begin e1 e2)
                (list ys (Begin e1 e2))]
               [(list 'if e1 e2 e3)
                (list ys (If e1 e2 e3))]
               [(list-rest g es)
                (list (cons g ys) (App (Var g) es))])])])]
      [(cons s sr)
       (match (parse-e/acc s xs ys)
         [(list ys e)
          (match (parse-es/acc sr xs ys)
            [(list ys es)
             (list ys (App e es))])])]
      [_
       (error "parse error" s)]))
  (rec s xs ys))
;; S-Expr [Listof Id] [Listof Id] [Listof Id] [Listof Id] -> (list [Listof Id] [Listof Expr])
;;   s: list of expressions shaped s-expr to be parsed
;;   xs: bound variables
;;   ys: free variables
;; returns list of free variables and parse of expressions
(define (parse-es/acc s xs ys)
  (match s
    ['() (list ys '())]
    [(cons s ss)
     (match (parse-e/acc s xs ys)
       [(list ys e)
        (match (parse-es/acc ss xs ys)
          [(list ys es)
           (list ys (cons e es))])])]
    [_ (error "parse error")]))
;; S-Expr [Listof Id] [Listof Id] [Listof Id] [Listof Id] -> (list [Listof Id] [Listof Expr])
;;   s: list of match clauses shaped s-expr to be parsed
;;   xs: bound variables
;;   ys: free variables
;; returns list of free variables and list of parsed clause patterns and clause expressions
(define (parse-match-clauses/acc sr xs ys)
  (match sr
    ['() (list ys '() '())]
    [(cons (list sp se) sr)
     (match (parse-match-pattern/acc sp xs ys)
       [(list ys xs p)
        (match (parse-e/acc se xs ys)
          [(list ys e)
           (match (parse-match-clauses/acc sr xs ys)
             [(list ys ps es)
              (list ys (cons p ps) (cons e es))])])])]))
;; S-Expr [Listof Id] [Listof Id] [Listof Id] [Listof Id] -> (list [Listof Id] [Listof Id] Pat)
;;   s: list of patterns shaped s-expr to be parsed
;;   xs: bound variables
;;   ys: free variables
;; returns list of free variables, bound variables, and parse of pattern
(define (parse-match-pattern/acc s xs ys)
  (define (rec p xs ys)
    (match p
      [(? datum?)  (list ys xs (Lit p))]
      ['_          (list ys xs (Var '_))]
      [(? symbol?) (list ys (cons p xs) (Var p))]
      [(list 'quote '())
       (list ys xs (Lit '()))]
      [(list 'box s)
       (match (rec s xs ys)
         [(list ys xs p)
          (list ys xs (Box p))])]
      [(list 'cons s1 s2)
       (match (rec s1 xs ys)
         [(list ys xs p1)
          (match (rec s2 xs ys)
            [(list ys xs p2)
             (list ys xs (Cons p1 p2))])])]
      [(list 'and s1 s2)
       (match (rec s1 xs ys)
         [(list ys xs p1)
          (match (rec s2 xs ys)
            [(list ys xs p2)
             (list ys xs (Conj p1 p2))])])]
      [_ (error "parse pattern error")]))
  (rec s xs ys))

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
(define (self-quoting-datum? x)
  (or (exact-integer? x)
      (boolean? x)
      (char? x)
      (string? x)))

;; Any -> Boolean
(define (datum? x)
  (or (self-quoting-datum? x)
      (empty? x)))

;; Any -> Boolean
(define (op0? x)
  (memq x '(read-byte peek-byte void)))

(define (op1? x)
  (memq x '(add1 sub1 zero?
                 char? integer->char char->integer
                 write-byte eof-object?
                 box unbox empty? cons? box? car cdr
                 vector? vector-length string? string-length)))

(define (op2? x)
  (memq x '(+ - < = eq? cons
              make-vector vector-ref make-string string-ref)))

(define (op3? x)
  (memq x '(vector-set!)))

