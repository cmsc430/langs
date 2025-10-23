#lang racket
(provide (all-defined-out))
(require ffi/unsafe)

(define imm-shift          3)
(define imm-mask       #b111)
(define ptr-mask       #b111)
(define type-box       #b001)
(define type-cons      #b010)
(define type-vect      #b011)
(define type-str       #b100)
(define type-proc      #b101)
(define int-shift (+ 1 imm-shift))
(define mask-int #b1111)
(define char-shift (+ 2 imm-shift))
(define type-int #b0000)
(define type-char #b01000)
(define mask-char #b11111)

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
        [else (error "invalid bits")]))

;; Value -> Integer
;; v must be an immediate
(define (value->bits v)
  (cond [(eq? v #t) #b00011000]
        [(eq? v #f) #b00111000]
        [(eq? v eof) #b01011000]
        [(eq? v (void)) #b01111000]
        [(eq? v '()) #b10011000]
        [(integer? v) (arithmetic-shift v int-shift)]
        [(char? v)
         (bitwise-ior type-char
                      (arithmetic-shift (char->integer v) char-shift))]
        [else (error "not an immediate value" v)]))

(define (int-bits? v)
  (= type-int (bitwise-and v mask-int)))

(define (char-bits? v)
  (= type-char (bitwise-and v mask-char)))

(define (imm-bits? v)
  (zero? (bitwise-and v imm-mask)))

(define (cons-bits? v)
  (= type-cons (bitwise-and v imm-mask)))

(define (box-bits? v)
  (= type-box (bitwise-and v imm-mask)))

(define (vect-bits? v)
  (= type-vect (bitwise-and v imm-mask)))

(define (str-bits? v)
  (= type-str (bitwise-and v imm-mask)))

(define (mem-ref i)
  (ptr-ref (cast i _int64 _pointer) _int64))

(define (mem-ref32 i)
  (ptr-ref (cast i _int64 _pointer) _int32))

(define (proc-bits? v)
  (= type-proc (bitwise-and v imm-mask)))

