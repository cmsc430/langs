#lang racket
(require "../syntax/parse.rkt")
(require "../compiler/compile.rkt")
(require "define-tests.rkt")
(require a86/interp)

(test (λ (e) (asm-interp (compile (parse e)))))

