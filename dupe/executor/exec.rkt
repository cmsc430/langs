#lang racket
(provide exec)
(require a86/interp)
(require "../compiler/compile.rkt")
(require "decode.rkt")

;; Expr -> Value
(define (exec e)
  (bits->value (asm-interp (compile e))))
