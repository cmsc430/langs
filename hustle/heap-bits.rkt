#lang racket
(require "types.rkt")
(provide (struct-out heap) heap-ref
         alloc-box alloc-cons)

(struct heap ([n #:mutable] bytes))

;; Value* Heap -> Value*
(define (alloc-box v h)
  (match h
    [(heap n bs)
     (heap-set! h n v)
     (set-heap-n! h (+ n 8))
     (bitwise-xor (arithmetic-shift n 16) type-mutable-box)]))

;; Value* Value* Heap -> Value*
(define (alloc-cons v1 v2 h)
  (match h
    [(heap n bs)
     (heap-set! h (+ n 0) v2)
     (heap-set! h (+ n 8) v1)
     (set-heap-n! h (+ n 16))
     (bitwise-xor (arithmetic-shift n 16) type-cons)]))

;; Heap Address -> Value*
(define (heap-ref h a)
  (integer-bytes->integer (heap-bytes h) #t #f a (+ a 8)))

;; Heap Address Value* -> Void
(define (heap-set! h i v)
  (integer->integer-bytes v 8 (negative? v) #f (heap-bytes h) i))

