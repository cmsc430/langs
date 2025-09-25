#lang racket
(provide interp)
(require "ast.rkt")
(require "interp-prim.rkt")
(require "env.rkt")

;; type Value =
;; | Integer
;; | Boolean
;; | Character
;; | Eof
;; | Void

;; type Answer = Value | 'err

;; type Env = (Listof (List Id Value))

(define (err? x) (eq? x 'err))
;; ClosedExpr -> Answer
(define (interp e)
  (with-handlers ([err? identity])
    (interp-e e '())))
;; Expr -> Value Env { raises 'err }
(define (interp-e e r) ;; where r closes e
  (match e
    [(Var x) (lookup r x)]
    [(Lit d) d]
    [(Eof)   eof]
    [(Prim0 p)
     (interp-prim0 p)]
    [(Prim1 p e)
     (interp-prim1 p (interp-e e r))]
    [(Prim2 p e1 e2)
     (interp-prim2 p
                   (interp-e e1 r)
                   (interp-e e2 r))]
    [(If e1 e2 e3)
     (if (interp-e e1 r)
         (interp-e e2 r)
         (interp-e e3 r))]
    [(Begin e1 e2)
     (begin (interp-e e1 r)
            (interp-e e2 r))]
    [(Let x e1 e2)
     (let ((v (interp-e e1 r)))
       (interp-e e2 (ext r x v)))]))

