#lang racket
(require a86/interp)
(require "run.rkt")
(require "compile.rkt")
(require "types.rkt")
(provide exec)
;; Expr -> Value
(define (exec e)
  (run (compile e)))

