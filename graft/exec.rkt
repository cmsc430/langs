#lang racket
(require a86/interp)
(require "compile.rkt")
(require "types.rkt")
(require "build-runtime.rkt")
(provide exec)
;; Expr -> Answer
(define (exec e)
  (parameterize ((current-objs (list (path->string runtime-path))))
    (match (asm-interp (compile e))
      ['err 'err]
      [b (bits->value b)])))

