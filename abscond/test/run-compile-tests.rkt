#lang racket
(require "../compiler/compile.rkt")
(require "../syntax/parse.rkt")
(require "define-tests.rkt")
(require a86/interp)

(test (λ (e) (asm-interp (compile (parse e)))))

