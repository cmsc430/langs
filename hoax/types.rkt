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
;; skip
(define type-vector         (ptr-type-enum #b100))
(define type-mutable-vector (ptr-type-enum #b101))
(define type-string         (ptr-type-enum #b110))
(define type-mutable-string (ptr-type-enum #b111))

(define type-immutable-box type-box)
(define type-immutable-vector type-vector)
(define type-immutable-string type-string)

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
        [(vect-bits? b)
         (build-vector (bits->value (mem-ref (vector-length-pointer b)))
                       (λ (j)
                         (bits->value (mem-ref (vector-ref-pointer b j)))))]
        [(str-bits? b)
         (build-string (bits->value (mem-ref (string-length-pointer b)))
                       (λ (j)
                         (bits->value (mem-ref (string-ref-pointer b j)))))]
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

(define (vect-bits? v)
  (or (= type-mutable-vector (bitwise-and v #xFFFF))
      (= type-immutable-vector (bitwise-and v #xFFFF))))

(define (str-bits? v)
   (or (= type-mutable-string (bitwise-and v #xFFFF))
       (= type-immutable-string (bitwise-and v #xFFFF))))

;; BoxValue* -> Address
(define (box-pointer v)
  (untag v))

;; ConsValue* -> Address
(define (cons-car-pointer v)
  (+ (untag v) 8))

;; ConsValue* -> Address
(define (cons-cdr-pointer v)
  (untag v))

;; VectValue* -> Address
(define (vector-length-pointer v)
  (untag v))

;; VectValue* Natural -> Address
(define (vector-ref-pointer v i)
  (+ (untag v) (* 8 (add1 i))))

;; StrtValue* -> Address
(define (string-length-pointer v)
  (untag v))

;; StrValue* Natural -> Address
(define (string-ref-pointer v i)
  (+ (untag v) (* 8 (add1 i))))

(define (untag i)
  (arithmetic-shift i -16))

(define (mem-ref i)
  (ptr-ref (cast i _int64 _pointer) _int64))

(define (char-ref i j)
  (integer->char (ptr-ref (cast (untag i) _int64 _pointer) _uint32 j)))

