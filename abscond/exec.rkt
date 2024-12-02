#lang racket
(require a86/interp)
(require "compile.rkt")
(provide exec)

;; Expr -> Integer
(define (exec e)
  (asm-interp (compile e)))

