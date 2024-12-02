#lang racket
(provide alloc-box alloc-cons heap-ref heap-set box-ptr cons-ptr)

(struct box-ptr (i))
(struct cons-ptr (i))

;; Value* Heap -> Answer*
(define (alloc-box v h)
  (cons (cons v h)
        (box-ptr (length h))))

;; Value* Value* Heap -> Answer*
(define (alloc-cons v1 v2 h)
  (cons (cons v2 (cons v1 h))
        (cons-ptr (length h))))

;; Heap Address -> Value*
(define (heap-ref h a)
  (list-ref h (- (length h) (add1 a))))

;; Heap Address Value* -> Heap
(define (heap-set h i v)
  (list-set h (- (length h) i 1) v))
