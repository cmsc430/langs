#lang racket
(require "types.rkt")
(provide (struct-out heap) heap-ref heap-set!
         alloc-box alloc-cons
         alloc-vect alloc-str)

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

;; [Listof Value*] Heap -> Value*
(define (alloc-vect vs h)
  (match h
    [(heap n bs)
     (heap-set! h n (arithmetic-shift (length vs) int-shift))
     (write-values! h vs (+ n 8))
     (set-heap-n! h (+ n (* 8 (add1 (length vs)))))
     (bitwise-xor (arithmetic-shift n 16) type-mutable-vector)]))

;; [Listof CharBits] Heap -> Value*
(define (alloc-str cs h)
  (match h
    [(heap n bs)
     (heap-set! h n (arithmetic-shift (length cs) int-shift))
     (write-values! h cs (+ n 8))
     (set-heap-n! h (+ n (* 8 (add1 (length cs)))))
     (bitwise-xor (arithmetic-shift n 16) type-mutable-string)]))


;; Heap [Listof Value*] Natural -> Void
(define (write-values! h vs i)
  (match vs
    ['() (void)]
    [(cons v vs)
     (heap-set! h i v)
     (write-values! h vs (+ i 8))]))

;; Heap Address -> Value*
(define (heap-ref h a)
  (integer-bytes->integer (heap-bytes h) #t #f a (+ a 8)))

;; Heap Address Natural -> [Listof Value*]
(define (heap-ref/n h a n)
  (define (loop n vs)
    (match n
      [0 vs]
      [_ (loop (sub1 n)
               (cons (heap-ref h (+ a n)) vs))]))
  (loop n '()))

;; Heap Address Value* -> Void
(define (heap-set! h i v)
  (integer->integer-bytes v 8 (negative? v) #f (heap-bytes h) i))

