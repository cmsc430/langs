#lang racket
(require a86/interp)
(require "run.rkt")
(require "compile.rkt")
(require "types.rkt")
(require "build-runtime.rkt")
(provide exec exec/io)
;; Expr -> Value
(define (exec e)
  (run (compile e)))

;; Expr String -> (cons Answer String)
(define (exec/io e in)
  (run/io (compile e) in))

