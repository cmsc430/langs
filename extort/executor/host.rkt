#lang racket

(require a86/interp)
(require ffi/unsafe)
(require "decode.rkt")
(require "../runtime/types.rkt")
(provide (all-defined-out))

(define (prim-read-byte)
  (value->bits (read-byte)))
(define (prim-peek-byte)
  (value->bits (peek-byte)))
(define (prim-write-byte bs)
  (value->bits (write-byte (bits->value bs))))
(define (prim-raise-error)
  (raise 'err))

(define (asm-interp/host asm)  
  (parameterize
      ([current-externs
        (list (extern 'read_byte prim-read-byte (_fun -> _int64))
              (extern 'peek_byte prim-peek-byte (_fun -> _int64))
              (extern 'write_byte prim-write-byte (_fun _int64 -> _int64))
              (extern 'raise_error prim-raise-error (_fun -> _void)))])
    (asm-interp asm)))


