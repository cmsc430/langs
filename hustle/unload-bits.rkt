#lang racket
(provide unload unload-value)
(require "heap-bits.rkt")
(require "types.rkt")

;; Heap Answer* -> Answer
(define (unload h a)
  (match a
    ['err 'err]
    [v (unload-value v h)]))

;; Value* Heap -> Value
(define (unload-value v h)
  (match v
    [(? box-bits?)
     (box (unload-value (heap-ref h (box-pointer v)) h))]
    [(? cons-bits?)
     (cons (unload-value (heap-ref h (cons-car-pointer v)) h)
           (unload-value (heap-ref h (cons-cdr-pointer v)) h))]
    [_ (bits->value v)]))

