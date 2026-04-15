#lang racket
(provide alloc-box alloc-cons heap-ref heap-set
         (struct-out box-ptr)
         (struct-out cons-ptr)
         heap-ref/n
         alloc-vect alloc-str
         (struct-out vect-ptr)
         (struct-out str-ptr))

(struct box-ptr (i))
(struct cons-ptr (i))
(struct vect-ptr (i))
(struct str-ptr (i))

;; Value* Heap -> Answer*
(define (alloc-box v h)
  (cons (cons v h)
        (box-ptr (length h))))

;; Value* Value* Heap -> Answer*
(define (alloc-cons v1 v2 h)
  (cons (cons v2 (cons v1 h))
        (cons-ptr (length h))))

;; [Listof Value*] Heap -> Answer*
(define (alloc-vect vs h)
  (cons (append (reverse (cons (length vs) vs)) h)
        (vect-ptr (length h))))

;; [Listof Char] Heap -> Answer*
(define (alloc-str cs h)
  (cons (append (reverse (cons (length cs) cs)) h)
        (str-ptr (length h))))

;; Heap Address -> Value*
(define (heap-ref h a)
  (list-ref h (- (length h) (add1 a))))

;; Heap Address Value* -> Heap
(define (heap-set h i v)
  (list-set h (- (length h) i 1) v))

;; Heap Address Natural -> [Listof Value*]
(define (heap-ref/n h a n)
  (define (loop n vs)
    (match n
      [0 vs]
      [_ (loop (sub1 n)
               (cons (heap-ref h (+ a n)) vs))]))
  (loop n '()))
