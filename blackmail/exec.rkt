#lang racket
(require a86/interp)
(require "run.rkt")
(require "compile.rkt")
(provide exec)

;; Expr -> Integer
(define (exec e)
  (run (compile e)))

