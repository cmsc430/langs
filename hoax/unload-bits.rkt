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
     (define p (bitwise-xor v type-box))
     (box (unload-value (heap-ref h p) h))]
    [(? cons-bits?)
     (define p (bitwise-xor v type-cons))
     (cons (unload-value (heap-ref h (+ p 0)) h)
           (unload-value (heap-ref h (+ p 8)) h))]
    [(? vect-bits?)
     (define p (bitwise-xor v type-vect))
     (build-vector (arithmetic-shift (heap-ref h p) (- int-shift))
                   (λ (i)
                     (bits->value (heap-ref h (+ p (* 8 (add1 i)))))))]
    [(? str-bits?)
     (define p (bitwise-xor v type-str))
     (build-string (arithmetic-shift (heap-ref h p) (- int-shift))
                   (λ (i)
                     (bits->value (heap-ref h (+ p (* 8 (add1 i)))))))]
    [_ (bits->value v)]))

