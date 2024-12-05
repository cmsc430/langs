#lang racket
(provide interp)
(require "ast.rkt")
(require "types.rkt")
(require "env.rkt")
(require "heap-bits.rkt")
(require "interp-prims-heap-bits.rkt")
(require "unload-bits.rkt")

(define *heap-size* 100000) ; # words in heap

;; type Answer* =
;; | Value*
;; | 'err

;; type Value* =
;; | (value->bits Integer)
;; | (value->bits Boolean)
;; | (value->bits Character)
;; | (value->bits Eof)
;; | (value->bits Void)
;; | (value->bits '())
;; | (bitwise-xor Address type-box)
;; | (bitwise-xor Address type-cons)

;; type Address = Natural divisible by 8

;; type Heap = (heap Address Bytes)

;; type REnv = (Listof (List Id Value*))

;; Expr -> Value
(define (interp e)
  (define h (heap 0 (make-bytes (* 8 *heap-size*) 0)))
  (unload h (interp-env-heap-bits e '() h)))

;; Expr REnv Heap -> Answer*
(define (interp-env-heap-bits e r h)
  (match e
    [(Lit (? string? s)) (alloc-str (map value->bits (string->list s)) h)]
    [(Lit d) (value->bits d)]
    [(Eof)   (value->bits eof)]
    [(Var x) (lookup r x)]
    [(Prim0 p) (interp-prim0 p h)]
    [(Prim1 p e)
     (match (interp-env-heap-bits e r h)
       ['err 'err]
       [v
        (interp-prim1 p v h)])]
    [(Prim2 p e1 e2)
     (match (interp-env-heap-bits e1 r h)
       ['err 'err]
       [v1
        (match (interp-env-heap-bits e2 r h)
          ['err 'err]
          [v2
           (interp-prim2 p v1 v2 h)])])]
    [(Prim3 p e1 e2 e3)
     (match (interp-env-heap-bits e1 r h)
       ['err 'err]
       [v1
        (match (interp-env-heap-bits e2 r h)
          ['err 'err]
          [v2
           (match (interp-env-heap-bits e3 r h)
             ['err 'err]
             [v3
              (interp-prim3 p v1 v2 v3 h)])])])]
    [(If p e1 e2)
     (match (interp-env-heap-bits p r h)
       ['err 'err]
       [v
        (if (= v (value->bits #f))
            (interp-env-heap-bits e2 r h)
            (interp-env-heap-bits e1 r h))])]
    [(Begin e1 e2)
     (match (interp-env-heap-bits e1 r h)
       ['err 'err]
       [_ (interp-env-heap-bits e2 r h)])]
    [(Let x e1 e2)
     (match (interp-env-heap-bits e1 r h)
       ['err 'err]
       [v
        (interp-env-heap-bits e2 (ext r x v) h)])]))

