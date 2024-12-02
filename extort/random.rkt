#lang racket
(provide random-expr random-well-defined-expr)
(require "parse.rkt")

;; Randomly generate an expression
;; Note: this will often generate programs with type errors
(define (random-expr)
  (parse (contract-random-generate expr/c)))

(define (random-well-defined-expr)
  (parse (contract-random-generate expr-good/c)))

(define op0/c
  (one-of/c 'read-byte 'peek-byte 'void))

(define op1/c
  (one-of/c 'add1 'sub1 'zero? 'char? 'integer->char 'char->integer
            'write-byte 'eof-object?))

(define expr/c
  (flat-rec-contract e
                     boolean?
                     char?
                     'eof
                     (integer-in #f #f)
                     (list/c op0/c)
                     (list/c op1/c e)
                     (list/c 'if e e e)
                     (list/c 'begin e e)))

(define expr-good/c
 (flat-murec-contract
  ([e-int e-byte
          (integer-in #f #f)
          (list/c 'add1 e-int)
          (list/c 'sub1 e-int)
          (list/c 'char->integer e-char)
          (list/c 'if e-any e-int e-int)
          (list/c 'begin e-any e-int)]
   [e-byte (integer-in 0 255)
           (list/c 'if
                  (list/c 'eof-object? (list/c 'peek-byte))
                  e-byte
                  (list/c 'read-byte))
           (list/c 'if
                  (list/c 'eof-object? (list/c 'peek-byte))
                  e-byte
                  (list/c 'peek-byte))
           (list/c 'if e-any e-byte e-byte)
           (list/c 'begin e-any e-byte)]
   [e-char char?
           (list/c 'integer->char e-codepoint)
           (list/c 'if e-any e-char e-char)
           (list/c 'begin e-any e-char)]
   [e-bool boolean?
           (list/c 'char? e-any)
           (list/c 'zero? e-int)
           (list/c 'eof-object? e-any)
           (list/c 'if e-any e-bool e-bool)
           (list/c 'begin e-any e-bool)]
   [e-codepoint (integer-in 0 #xD7FF)
                (integer-in #xE000 #x10FFFF)
                (list/c 'if e-any e-codepoint e-codepoint)
                (list/c 'begin e-any e-codepoint)]
   [e-void (list/c 'void)
           (list/c 'write-byte e-byte)
           (list/c 'if e-any e-void e-void)
           (list/c 'begin e-any e-void)]
   [e-any e-int e-byte e-char e-bool e-codepoint e-void
          'eof
          (list/c 'if e-any e-any e-any)
          (list/c 'begin e-any e-any)])
  e-any))
