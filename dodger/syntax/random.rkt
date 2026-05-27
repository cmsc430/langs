#lang racket
(provide random-expr random-well-defined-expr)
(require "parse.rkt")

;; Randomly generate an expression
;; Note: this will often generate programs with type errors
(define (random-expr)
  (parse (contract-random-generate expr/c)))

(define (random-well-defined-expr)
  (parse (contract-random-generate expr-good/c)))

(define op1/c
  (one-of/c 'add1 'sub1 'zero? 'char? 'integer->char 'char->integer))

(define expr/c
  (flat-rec-contract e
                     boolean?
                     char?
                     (integer-in #f #f)
                     (list/c op1/c e)
                     (list/c 'if e e e)))

(define expr-good/c
 (flat-murec-contract
  ([e-int (integer-in #f #f)
          (list/c 'add1 e-int)
          (list/c 'sub1 e-int)
          (list/c 'char->integer e-char)
          (list/c 'if e-any e-int e-int)]
   [e-char char?
           (list/c 'integer->char e-codepoint)
           (list/c 'if e-any e-char e-char)]
   [e-bool boolean?
           (list/c 'char? e-any)
           (list/c 'zero? e-int)
           (list/c 'if e-any e-bool e-bool)]
   [e-codepoint (integer-in 0 #xD7FF)
                (integer-in #xE000 #x10FFFF)]
   [e-any e-int e-char e-bool
          (list/c 'if e-any e-any e-any)])
  e-any))
