#lang racket
(provide interp-prim0 interp-prim1 interp-prim2 interp-prim3)
(require "heap-bits.rkt")
(require "types.rkt")

;; Op0 Heap -> Answer*
(define (interp-prim0 op h)
  (match op
    ['read-byte (value->bits (read-byte))]
    ['peek-byte (value->bits (peek-byte))]
    ['void      (value->bits (void))]))

;; Op1 Value* Heap -> Answer*
(define (interp-prim1 p v h)
  (match (list p v)
    [(list 'add1 (? int-bits? i))
     (+ i (value->bits 1))]
    [(list 'sub1 (? int-bits? i))
     (- i (value->bits 1))]
    [(list 'zero? (? int-bits? i))
     (value->bits (zero? i))]
    [(list 'char? v)
     (value->bits (char-bits? v))]
    [(list 'char->integer (? char-bits?))
     (arithmetic-shift (bitwise-xor v type-char) (- int-shift char-shift))]
    [(list 'integer->char (? codepoint-bits?))
     (bitwise-xor (arithmetic-shift v (- char-shift int-shift)) type-char)]
    [(list 'eof-object? v)
     (value->bits (= (value->bits eof) v))]
    [(list 'write-byte (? byte-bits?))
     (begin (write-byte (arithmetic-shift v (- int-shift)))
            (value->bits (void)))]
    [(list 'box v) (alloc-box v h)]
    [(list 'unbox (? box-bits? i))
     (heap-ref h (box-pointer i))]
    [(list 'car (? cons-bits? i))
     (heap-ref h (cons-car-pointer i))]
    [(list 'cdr (? cons-bits? i))
     (heap-ref h (cons-cdr-pointer i))]
    [(list 'empty? v)
     (value->bits (= (value->bits '()) v))]
    [(list 'vector? v)
     (value->bits (vect-bits? v))]
    [(list 'string? v)
     (value->bits (str-bits? v))]
    [(list 'vector-length (? vect-bits?))
     (heap-ref h (vector-length-pointer v))]
    [(list 'string-length (? str-bits?))
     (heap-ref h (string-length-pointer v))]
    [_ 'err]))

;; Op2 Value* Value* Heap -> Answer*
(define (interp-prim2 p v1 v2 h)
  (match (list p v1 v2)
    [(list '+ (? int-bits? i1) (? int-bits? i2)) (+ i1 i2)]
    [(list '- (? int-bits? i1) (? int-bits? i2)) (- i1 i2)]
    [(list '< (? int-bits? i1) (? int-bits? i2)) (value->bits (< i1 i2))]
    [(list '= (? int-bits? i1) (? int-bits? i2)) (value->bits (= i1 i2))]
    [(list 'eq? v1 v2) (value->bits (= v1 v2))]
    [(list 'cons v1 v2) (alloc-cons v1 v2 h)]
    [(list 'make-vector (? int-bits? i) v)
     (if (< i 0)
         'err
         (alloc-vect (make-list (arithmetic-shift i (- int-shift)) v) h))]
    [(list 'vector-ref (? vect-bits? a) (? int-bits? i))
     (if (<= 0 i (sub1 (heap-ref h (vector-length-pointer a))))
         (heap-ref h (vector-ref-pointer a (arithmetic-shift i (- int-shift))))
         'err)]
    [(list 'make-string (? int-bits? i) (? char-bits? c))
     (if (< i 0)
         'err
         (alloc-str (make-list (arithmetic-shift i (- int-shift)) c) h))]
    [(list 'string-ref (? str-bits? a) (? int-bits? i))
     (if (<= 0 i (sub1 (heap-ref h (string-length-pointer a))))
         (heap-ref h (string-ref-pointer a (arithmetic-shift i (- int-shift))))
         'err)]
    [_ 'err]))

;; Op3 Value* Value* Value* Heap -> Answer*
(define (interp-prim3 p v1 v2 v3 h)
  (match (list p v1 v2 v3)
    [(list 'vector-set! (? vect-bits?) (? int-bits?) _)
     (if (<= 0 v2 (sub1 (heap-ref h (vector-length-pointer v1))))
         (heap-set! h (vector-ref-pointer v1 (arithmetic-shift v2 (- int-shift))) v3)
         'err)]
    [_ 'err]))

;; Int64 -> Boolean
(define (byte-bits? v)
  (and (int-bits? v)
       (<= 0 v (value->bits 255))))

;; Int64 -> Boolean
(define (codepoint-bits? v)
  (and (int-bits? v)
       (or (<= 0 v (value->bits 55295))
           (<= (value->bits 57344) v (value->bits 1114111)))))

