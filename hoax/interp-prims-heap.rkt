#lang racket
(provide interp-prim0 interp-prim1 interp-prim2 interp-prim3)
(require "heap.rkt")

;; Op0 Heap -> Answer*
(define (interp-prim0 op h)
  (match op
    ['read-byte (cons h (read-byte))]
    ['peek-byte (cons h (peek-byte))]
    ['void      (cons h (void))]))

;; Op1 Value* Heap -> Answer*
(define (interp-prim1 p v h)
  (match (list p v)
    [(list 'add1 (? integer? i))          (cons h (add1 i))]
    [(list 'sub1 (? integer? i))          (cons h (sub1 i))]
    [(list 'zero? (? integer? i))         (cons h (zero? i))]
    [(list 'char? v)                      (cons h (char? v))]
    [(list 'char->integer (? char?))      (cons h (char->integer v))]
    [(list 'integer->char (? codepoint?)) (cons h (integer->char v))]
    [(list 'eof-object? v)                (cons h (eof-object? v))]
    [(list 'write-byte (? byte?))         (cons h (write-byte v))]
    [(list 'box v)                        (alloc-box v h)]
    [(list 'unbox (box-ptr i))            (cons h (heap-ref h i))]
    [(list 'car   (cons-ptr i))           (cons h (heap-ref h i))]
    [(list 'cdr   (cons-ptr i))           (cons h (heap-ref h (add1 i)))]
    [(list 'empty? v)                     (cons h (empty? v))]
    [(list 'vector? v)                    (cons h (vect-ptr? v))]
    [(list 'string? v)                    (cons h (str-ptr? v))]
    [(list 'vector-length (vect-ptr a))   (cons h (heap-ref h a))]
    [(list 'string-length (str-ptr a))    (cons h (heap-ref h a))]
    [_                                    'err]))

;; Op2 Value* Value* Heap -> Answer*
(define (interp-prim2 p v1 v2 h)
  (match (list p v1 v2)
    [(list '+ (? integer? i1) (? integer? i2)) (cons h (+ i1 i2))]
    [(list '- (? integer? i1) (? integer? i2)) (cons h (- i1 i2))]
    [(list '< (? integer? i1) (? integer? i2)) (cons h (< i1 i2))]
    [(list '= (? integer? i1) (? integer? i2)) (cons h (= i1 i2))]
    [(list 'eq? v1 v2)
     (match (list v1 v2)
       [(list (cons-ptr a1) (cons-ptr a2)) (cons h (= a1 a2))]
       [(list (box-ptr a1)  (box-ptr a2))  (cons h (= a1 a2))]
       [_                                  (cons h (eqv? v1 v2))])]
    [(list 'cons v1 v2) (alloc-cons v1 v2 h)]
    [(list 'make-vector (? integer? i) v)
     (if (< i 0)
         'err
         (alloc-vect (make-list i v) h))]
    [(list 'vector-ref (vect-ptr a) (? integer? i))
     (if (<= 0 i (sub1 (heap-ref h a)))
         (cons h (heap-ref h (+ a i 1)))
         'err)]
    [(list 'make-string (? integer? i) (? char? c))
     (if (< i 0)
         'err
         (alloc-str (make-list i c) h))]
    [(list 'string-ref (str-ptr a) (? integer? i))
     (if (<= 0 i (sub1 (heap-ref h a)))
         (cons h (heap-ref h (+ a i 1)))
         'err)]
    [_ 'err]))

;; Op3 Value* Value* Value* Heap -> Answer*
(define (interp-prim3 p v1 v2 v3 h)
  (match (list p v1 v2 v3)
    [(list 'vector-set! (vect-ptr a) (? integer? i) v)
     (if (<= 0 i (sub1 (heap-ref h a)))
         (cons (heap-set h (+ a i 1) v)
               (void))
         'err)]
    [_ 'err]))

;; Any -> Boolean
(define (codepoint? v)
  (and (integer? v)
       (or (<= 0 v 55295)
           (<= 57344 v 1114111))))

