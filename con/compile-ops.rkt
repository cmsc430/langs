#lang racket
(provide compile-op1)
(require "ast.rkt")
(require a86/ast a86/registers)

;; Op1 -> Asm
(define (compile-op1 p)
  (match p
    ['add1 (Add rax 1)]
    ['sub1 (Sub rax 1)]))

