#lang racket
(require a86/interp)
(require "compile.rkt")
(require "types.rkt")
(provide exec)
;; Expr -> Value
(define (exec e)
  (bits->value (asm-interp (compile e))))

