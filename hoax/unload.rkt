#lang racket
(provide unload unload-value)
(require "heap.rkt")

;; Answer* -> Answer
(define (unload a)
  (match a
    ['err 'err]
    [(cons h v) (unload-value v h)]))

;; Value* Heap -> Value
(define (unload-value v h)
  (match v
    [(? integer?)    v]
    [(? boolean?)    v]
    [(? char?)       v]
    [(? eof-object?) v]
    [(? void?)       v]
    ['()             '()]
    [(box-ptr a)
     (box (unload-value (heap-ref h a) h))]
    [(cons-ptr a)
     (cons (unload-value (heap-ref h a) h)
           (unload-value (heap-ref h (add1 a)) h))]
    [(vect-ptr a)
     (apply vector (heap-ref/n h a (heap-ref h a)))]
    [(str-ptr a)
     (apply string (heap-ref/n h a (heap-ref h a)))]))
