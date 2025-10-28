#lang racket
(provide interp interp-e)
(provide interp-match-pat)
(require "ast.rkt")
(require "interp-prim.rkt")
(require "env.rkt")

;; type Value =
;; | Integer
;; | Boolean
;; | Character
;; | Eof
;; | Void
;; | '()
;; | (cons Value Value)
;; | (box Value)
;; | (string Character ...)
;; | (vector Value ...)
;; | (Value ... -> Answer)

;; type Answer = Value | 'err

;; type Env = (Listof (List Id Value))

(define (err? x) (eq? x 'err))
;; ClosedExpr -> Answer
;; Prog -> Answer
(define (interp p)
  (with-handlers ([err? identity])
    (match p
      [(Prog ds e)
       (interp-e e '() ds)])))
;l Expr Env Defns -> Value { raises 'err }
(define (interp-e e r ds) ;; where r closes e
  (match e
    [(Var x) (interp-var x r ds)]
    [(Lit d) d]
    [(Eof)   eof]
    [(Prim0 p)
     (interp-prim0 p)]
    [(Prim1 p e)
     (interp-prim1 p (interp-e e r ds))]
    [(Prim2 p e1 e2)
     (interp-prim2 p
                   (interp-e e1 r ds)
                   (interp-e e2 r ds))]
    [(Prim3 p e1 e2 e3)
     (interp-prim3 p
                   (interp-e e1 r ds)
                   (interp-e e2 r ds)
                   (interp-e e3 r ds))]
    [(If e1 e2 e3)
     (if (interp-e e1 r ds)
         (interp-e e2 r ds)
         (interp-e e3 r ds))]
    [(Begin e1 e2)
     (begin (interp-e e1 r ds)
            (interp-e e2 r ds))]
    [(Let x e1 e2)
     (let ((v (interp-e e1 r ds)))
       (interp-e e2 (ext r x v) ds))]
    [(App e es)
     (let ((f (interp-e e r ds))
           (vs (interp-e* es r ds)))
       (if (procedure? f)
           (apply f vs)
           (raise 'err)))]
    [(Match e ps es)
     (let ((v (interp-e e r ds)))
       (interp-match v ps es r ds))]
    [(Lam f xs e)
     (λ vs
       ; check arity matches
       (if (= (length xs) (length vs))
           (interp-e e (append (zip xs vs) r) ds)
           (raise 'err)))]))

;; (Listof Expr) REnv Defns -> (Listof Value) { raises 'err }
(define (interp-e* es r ds)
  (match es
    ['() '()]
    [(cons e es)
     (cons (interp-e e r ds)
           (interp-e* es r ds))]))

;; Id Env [Listof Defn] -> Answer
(define (interp-var x r ds)
  (match (lookup r x)
    ['err (match (defns-lookup ds x)
            [(Defn f xs e) (interp-e (Lam f xs e) '() ds)]
            [#f 'err])]
    [v v]))

;; Value [Listof Pat] [Listof Expr] Env Defns -> Answer
(define (interp-match v ps es r ds)
  (match* (ps es)
    [('() '()) 'err]
    [((cons p ps) (cons e es))
     (match (interp-match-pat p v r)
       [#f (interp-match v ps es r ds)]
       [r  (interp-e e r ds)])]))
;; Pat Value Env -> [Maybe Env]
(define (interp-match-pat p v r)
  (match p
    [(Var '_) r]
    [(Var x) (ext r x v)]
    [(Lit l) (and (eqv? l v) r)]
    [(Box p)
     (match v
       [(box v)
        (interp-match-pat p v r)]
       [_ #f])]
    [(Cons p1 p2)
     (match v
       [(cons v1 v2)
        (match (interp-match-pat p1 v1 r)
          [#f #f]
          [r1 (interp-match-pat p2 v2 r1)])]
       [_ #f])]
    [(Conj p1 p2)
     (match (interp-match-pat p1 v r)
       [#f #f]
       [r1 (interp-match-pat p2 v r1)])]))

;; Defns Symbol -> Defn
(define (defns-lookup ds f)
  (findf (match-lambda [(Defn g _ _) (eq? f g)])
         ds))

(define (zip xs ys)
  (match* (xs ys)
    [('() '()) '()]
    [((cons x xs) (cons y ys))
     (cons (list x y)
           (zip xs ys))]))

