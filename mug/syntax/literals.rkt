#lang racket
(provide literals symbols)

(require "ast.rkt")


;; Prog -> [Listof Symbol]
(define (literals p)
  (remove-duplicates
   (map to-symbol (literals* p))))

;; Prog -> [Listof Symbol]
(define (symbols p)
  (remove-duplicates (filter symbol? (literals* p))))

;; (U String Symbol) -> Symbol
(define (to-symbol s)
  (if (string? s)
      (string->symbol s)
      s))

;; Prog -> [Listof (U Symbol String)]
(define (literals* p)
  (match p
    [(Prog ds e)
     (append (append-map literals-d ds) (literals-e e))]))

;; Defn -> [Listof (U Symbol String)]
(define (literals-d d)
  (match d
    [(Defn f xs e)
     (literals-e e)]))

;; Expr -> [Listof (U Symbol String)]
(define (literals-e e)
  (match e
    [(Lit (? symbol? s)) (list s)]
    [(Lit (? string? s)) (list s)]
    [(Lit _) '()]
    [(Prim1 p e)
     (literals-e e)]
    [(Prim2 p e1 e2)
     (append (literals-e e1) (literals-e e2))]
    [(Prim3 p e1 e2 e3)
     (append (literals-e e1) (literals-e e2) (literals-e e3))]
    [(If e1 e2 e3)
     (append (literals-e e1) (literals-e e2) (literals-e e3))]
    [(Begin e1 e2)
     (append (literals-e e1) (literals-e e2))]
    [(Let x e1 e2)
     (append (literals-e e1) (literals-e e2))]
    [(App e1 es)
     (append (literals-e e1) (append-map literals-e es))]
    [(Lam f xs e)
     (literals-e e)]
    [(Match e ps es)
     (append (literals-e e) (append-map literals-match-clause ps es))]
    [_ '()]))

;; Pat Expr -> [Listof (U Symbol String)]
(define (literals-match-clause p e)
  (append (literals-pat p) (literals-e e)))

;; Pat -> [Listof (U Symbol String)]
(define (literals-pat p)
  (match p
    [(Lit (? symbol? s)) (list s)]
    [(Lit (? string? s)) (list s)]
    [(Box p) (literals-pat p)]
    [(Cons p1 p2) (append (literals-pat p1) (literals-pat p2))]    
    [(Conj p1 p2) (append (literals-pat p1) (literals-pat p2))]
    [_ '()]))

