#lang racket
(provide interp interp-e (struct-out Closure))
(require "../syntax/ast.rkt")
(require "interp-prim.rkt")
(require "env.rkt")
(require "../syntax/fv.rkt")

;; type Answer = Value | 'err

;; type Value = ...
;; | (Closure [Listof Id] Expr Env)
(struct Closure (xs e r) #:prefab)

;; type Env = (Listof (List Id Value))

;; type Defns = (Listof Defn)

(define (err? x) (eq? x 'err))

;; Prog -> Answer
(define (interp p)
  (with-handlers ([err? identity])
    (match p
      [(Prog ds e)
       (interp-e e '() ds)])))

;; Expr Env Defns -> Value { raises 'err }
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
    [(Match e ps es)
     (let ((v (interp-e e r ds)))
       (interp-match v ps es r ds))]
    
    
    [(Lam f xs e)
     (Closure xs e (restrict r (fv (Lam f xs e))))]
    [(App e es)
     (let ((f (interp-e e r ds))
           (vs (interp-e* es r ds)))
       (match f
         [(Closure xs e r)
          ; check arity matches
          (if (= (length xs) (length vs))           
              (interp-e e (append (zip xs vs) r) ds)
              (raise 'err))]
         [_ (raise 'err)]))]))

;; Env [Listof Id] -> Env
(define (restrict r xs)
    (match xs
      ['() '()]
      [(cons x xs)
       (cons (list x (lookup r x))
             (restrict r xs))]))

;; (Listof Expr) Env Defns -> (Listof Value) { raises 'err }
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

