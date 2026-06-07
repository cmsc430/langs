#lang racket

(require a86/interp)
(require a86/registers a86/ast)
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

(define heap
  (malloc 10000))

(define (asm-interp/host asm)  
  (parameterize
      ([current-externs
        (list (extern 'read_byte prim-read-byte (_fun -> _int64))
              (extern 'peek_byte prim-peek-byte (_fun -> _int64))
              (extern 'write_byte prim-write-byte (_fun _int64 -> _int64))
              (extern 'raise_error prim-raise-error (_fun -> _void)))])
    (let ((l (gensym)))
      (asm-interp
       (prog (Global l)
             (Label l)
             (Mov rdi (cast heap _pointer _int64))
             (Jmp 'entry)
             asm)))))


