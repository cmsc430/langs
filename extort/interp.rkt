#lang racket
(provide interp)
(require "ast.rkt")
(require "interp-prim.rkt")

;; type Value =
;; | Integer
;; | Boolean
;; | Character
;; | Eof
;; | Void

;; type Answer = Value | 'err
;; Expr -> Answer
(define (interp e)
  (match e
    [(Lit d) d]
    [(Eof)   eof]
    [(Prim0 p)
     (interp-prim0 p)]
    [(Prim1 p e)
     (match (interp e)
       ['err 'err]
       [v (interp-prim1 p v)])]
    [(If e1 e2 e3)
     (match (interp e1)
       ['err 'err]
       [v (if v
              (interp e2)
              (interp e3))])]
    [(Begin e1 e2)
     (match (interp e1)
       ['err 'err]
       [v (interp e2)])]))

