#lang racket
(provide interp interp-env-heap)
(require "env.rkt")
(require "unload.rkt")
(require "interp-prims-heap.rkt")
(require "ast.rkt")
(require "heap.rkt")

;; type Answer* =
;; | (cons Heap Value*)
;; | 'err

;; type Value* =
;; | Integer
;; | Boolean
;; | Character
;; | Eof
;; | Void
;; | '()
;; | (box-ptr  Address)
;; | (cons-ptr Address)
;; | (vect-ptr Address)
;; | (str-ptr Address)

;; type Address = Natural

;; type Heap = (Listof Value*)
;; type REnv = (Listof (List Id Value*))

;; Expr -> Answer
(define (interp e)
  (unload (interp-env-heap e '() '())))

;; Expr REnv Heap -> Answer*
(define (interp-env-heap e r h)
  (match e
    [(Lit (? string? s)) (alloc-str (string->list s) h)]
    [(Lit d)  (cons h d)]
    [(Eof)    (cons h eof)]
    [(Var x)  (cons h (lookup r x))]
    [(Prim0 p) (interp-prim0 p h)]
    [(Prim1 p e)
     (match (interp-env-heap e r h)
       ['err 'err]
       [(cons h v)
        (interp-prim1 p v h)])]
    [(Prim2 p e1 e2)
     (match (interp-env-heap e1 r h)
       ['err 'err]
       [(cons h v1)
        (match (interp-env-heap e2 r h)
          ['err 'err]
          [(cons h v2)
           (interp-prim2 p v1 v2 h)])])]
    [(Prim3 p e1 e2 e3)
     (match (interp-env-heap e1 r h)
       ['err 'err]
       [(cons h v1)
        (match (interp-env-heap e2 r h)
          ['err 'err]
          [(cons h v2)
           (match (interp-env-heap e3 r h)
             [(cons h v3)
              (interp-prim3 p v1 v2 v3 h)])])])]
    [(If p e1 e2)
     (match (interp-env-heap p r h)
       ['err 'err]
       [(cons h v)
        (if v
            (interp-env-heap e1 r h)
            (interp-env-heap e2 r h))])]
    [(Begin e1 e2)
     (match (interp-env-heap e1 r h)
       ['err 'err]
       [(cons h _) (interp-env-heap e2 r h)])]
    [(Let x e1 e2)
     (match (interp-env-heap e1 r h)
       ['err 'err]
       [(cons h v)
        (interp-env-heap e2 (ext r x v) h)])]))

