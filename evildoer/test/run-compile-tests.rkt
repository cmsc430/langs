#lang racket
(require "../compiler/compile.rkt")
(require "../syntax/parse.rkt")
(require "../executor/run.rkt")
(require "define-tests.rkt")

(test (λ (e) (run (compile (parse e)))))

(test/io (λ (i e) (run/io (compile (parse e)) i)))

