#lang racket
(provide run exec)
(require a86/interp)
(require "../compiler/compile.rkt")
(require "decode.rkt")
;; Asm -> Value
(define (run asm)
  (bits->value (asm-interp asm)))
;; Expr -> Value
(define (exec e)
  (run (compile e)))

