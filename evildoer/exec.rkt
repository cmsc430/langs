#lang racket
(require a86/interp)
(require "compile.rkt")
(require "types.rkt")
(require "build-runtime.rkt")
(provide exec)
;; Expr -> Value
(define (exec e)
  (parameterize ((current-objs (list (path->string runtime-path))))
    (bits->value (asm-interp (compile e)))))

