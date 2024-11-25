#lang racket
(require a86/interp)
(require "compile.rkt")
(require "interp.rkt")
(provide exec)

;; Expr -> Integer
(define (exec e)
  (asm-interp (compile e)))

