#lang racket
(provide run exec)
(require a86/interp)
(require "../compiler/compile.rkt")

;; Asm -> Integer
(define (run asm)
  (asm-interp asm))

;; Expr -> Integer
(define (exec e)
  (run (compile e)))

