#lang racket
(require a86/interp)
(require "run.rkt")
(require "compile.rkt")
(require "types.rkt")
(require "build-runtime.rkt")
(provide exec exec/io)
;; Prog -> Answer
(define (exec p)
  (run (compile p)))
;; Prog String -> (cons Answer String)
(define (exec/io p in)
  (run/io (compile p) in))

