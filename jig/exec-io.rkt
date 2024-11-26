#lang racket
(require a86/interp)
(require "compile.rkt")
(require "types.rkt")
(require "build-runtime.rkt")
(provide exec/io)
;; Prog String -> (cons Answer String)
(define (exec/io p in)
  (parameterize ((current-objs (list (path->string runtime-path))))
    (match (asm-interp/io (compile p) in)
      [(cons 'err o) (cons 'err o)]
      [(cons b o) (cons (bits->value b) o)])))

