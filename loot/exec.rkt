#lang racket
(require a86/interp)
(require "compile.rkt")
(require "types.rkt")
(require "build-runtime.rkt")
(provide exec)
;; Prog -> Answer
(define (exec p)
  (parameterize ((current-objs (list (path->string runtime-path))))
    (match (asm-interp (compile p))
      ['err 'err]
      [b (bits->value b)])))

