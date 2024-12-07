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
    [(? vect-bits?)
     (build-vector
      (arithmetic-shift (heap-ref h (vector-length-pointer v)) (- int-shift))
      (λ (i)
        (bits->value (heap-ref h (vector-ref-pointer v i)))))]
    [(? str-bits?)
     (build-string
      (arithmetic-shift (heap-ref h (string-length-pointer v)) (- int-shift))
      (λ (i)
        (bits->value (heap-ref h (string-ref-pointer v i)))))]
    [_ (bits->value v)]))

