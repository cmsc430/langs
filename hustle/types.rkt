#lang racket
(provide (all-defined-out))
(require ffi/unsafe)

(define imm-shift 1)
(define ptr-type-tag 1)

(define (ptr-type-enum i)
  (bitwise-xor (arithmetic-shift i imm-shift) ptr-type-tag))

;; Mutable and immutable must differ only in the lsb of the tag

(define zero-mut #xFFFD) ; use to zero out mutability bit

(define type-box            (ptr-type-enum #b000))
(define type-mutable-box    (ptr-type-enum #b001))
(define type-cons           (ptr-type-enum #b010))

(define type-immutable-box type-box)

(define imm-mask         #b1)
(define int-shift (+ 1 imm-shift))

(define mask-int #b11)

(define char-shift (+ 2 imm-shift))
(define type-int #b00)
(define type-char #b010)
(define mask-char #b111)

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
         (box (bits->value (mem-ref (box-pointer b))))]
        [(cons-bits? b)
         (cons (bits->value (mem-ref (cons-car-pointer b)))
               (bits->value (mem-ref (cons-cdr-pointer b))))]
        [else (error "invalid bits")]))

(define (value->bits v)
  (cond [(eq? v #t) #b000110]
        [(eq? v #f) #b001110]
        [(integer? v) (arithmetic-shift v int-shift)]
        [(eof-object? v) #b010110]
        [(void? v) #b011110]
        [(empty? v)      #b100110]
        [(char? v)
         (bitwise-xor type-char
                      (arithmetic-shift (char->integer v) char-shift))]
        [else (error "not an immediate value" v)]))

(define (int-bits? v)
  (= type-int (bitwise-and v mask-int)))

(define (char-bits? v)
  (= type-char (bitwise-and v mask-char)))

(define (imm-bits? v)
  (zero? (bitwise-and v imm-mask)))

(define (cons-bits? v)
  (= type-cons (bitwise-and v #xFFFF)))

(define (box-bits? v)
  (or (= type-mutable-box (bitwise-and v #xFFFF))
      (= type-immutable-box (bitwise-and v #xFFFF))))

;; BoxValue* -> Address
(define (box-pointer v)
  (untag v))

;; ConsValue* -> Address
(define (cons-car-pointer v)
  (+ (untag v) 8))

;; ConsValue* -> Address
(define (cons-cdr-pointer v)
  (untag v))

(define (untag i)
  (arithmetic-shift i -16))

(define (mem-ref i)
  (ptr-ref (cast i _int64 _pointer) _int64))

