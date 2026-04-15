#lang racket
(provide unload unload-value)
(require "heap-bits.rkt")
(require "../runtime/types.rkt")
(require "../executor/decode.rkt")

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
    [_ (bits->value v)]))

