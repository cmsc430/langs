#lang racket

(require "../runtime/types.rkt")
(require ffi/unsafe)

(provide (all-defined-out))

;; Integer -> Value
(define (bits->value b)
  (cond [(= b (value->bits #t))  #t]
        [(= b (value->bits #f)) #f]
        [(= b (value->bits eof))  eof]
        [(= b (value->bits (void))) (void)]
        [(int-bits? b)
         (arithmetic-shift b (- int-shift))]
        [(char-bits? b)
         (integer->char (arithmetic-shift b (- char-shift)))]
        [else (error "invalid bits")]))

(define _val
  (make-ctype _int64 value->bits bits->value))

