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

        [(vect-bits? b)
         (let ((p (- b type-vect)))
           (build-vector (bits->value (mem-ref p))
                         (lambda (j)
                           (bits->value (mem-ref (+ p (* 8 (add1 j))))))))]
        [(str-bits? b)
         (let ((p (- b type-str)))
           (build-string (bits->value (mem-ref p))
                         (lambda (j)
                           (integer->char (mem-ref32 (+ p 8 (* 4 j)))))))]
        [(proc-bits? b)
         (lambda _
           (error "This function is not callable."))]
        [(symb-bits? b)
         (let ((p (- b type-symb)))
           (string->symbol
            (build-string (bits->value (mem-ref p))
                          (lambda (j)
                            (integer->char (mem-ref32 (+ p 8 (* 4 j))))))))]
        [else (error "invalid bits")]))

(define (mem-ref i)
  (ptr-ref (cast i _int64 _pointer) _int64))

(define (mem-ref32 i)
  (ptr-ref (cast i _int64 _pointer) _int32))

(define _val
  (make-ctype _int64 value->bits bits->value))

