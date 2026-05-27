#lang racket
(provide bits->value _val)

(require "../runtime/types.rkt"
         ffi/unsafe)

(struct struct-val () #:transparent)

(define (bits->value b)
  (cond [(= b (value->bits #t))     #t]
        [(= b (value->bits #f))     #f]
        [(= b (value->bits eof))    eof]
        [(= b (value->bits (void))) (void)]
        [(= b (value->bits '()))    '()]
        [(int-bits? b)
         (arithmetic-shift b (- int-shift))]
        [(char-bits? b)
         (integer->char (arithmetic-shift b (- char-shift)))]
        [(box-bits? b)
         (box (bits->value (heap-ref b)))]
        [(cons-bits? b)
         (cons (bits->value (heap-ref (+ b 8)))
               (bits->value (heap-ref b)))]
        [(vect-bits? b)
         (if (zero? (untag b))
             (vector)
             (build-vector (heap-ref b)
                           (lambda (j)
                             (bits->value (heap-ref (+ b (* 8 (add1 j))))))))]
        [(str-bits? b)
         (if (zero? (untag b))
             (string)
             (build-string (heap-ref b)
                           (lambda (j)
                             (char-ref (+ b 8) j))))]
        [(symb-bits? b)
         (string->symbol
          (if (zero? (untag b))
              (string)
              (build-string (heap-ref b)
                            (lambda (j)
                              (char-ref (+ b 8) j)))))]
        [(struct-bits? b)
         (struct-val)]
        [(proc-bits? b)
         (lambda _
           (error "This function is not callable."))]
        [else (error "invalid bits")]))

(define (heap-ref i)
  (ptr-ref (cast (untag i) _int64 _pointer) _int64))

(define (char-ref i j)
  (integer->char (ptr-ref (cast (untag i) _int64 _pointer) _uint32 j)))

(define _val
  (make-ctype _int64 value->bits bits->value))
