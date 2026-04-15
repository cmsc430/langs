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
        [(= b (value->bits '())) '()]
        [(int-bits? b)
         (arithmetic-shift b (- int-shift))]
        [(char-bits? b)
         (integer->char (arithmetic-shift b (- char-shift)))]
        [(box-bits? b)
         (box (bits->value (mem-ref (- b type-box))))]
        [(cons-bits? b)
         (cons (bits->value (mem-ref (+ 0 (- b type-cons))))
               (bits->value (mem-ref (+ 8 (- b type-cons)))))]
        [else (error "invalid bits")]))

(define (mem-ref i)
  (ptr-ref (cast i _int64 _pointer) _int64))

(define _val
  (make-ctype _int64 value->bits bits->value))

