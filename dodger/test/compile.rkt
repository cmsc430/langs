#lang racket
(require "../compiler/compile.rkt")
(require "../syntax/parse.rkt")
(require "../executor/run.rkt")
(require "test-runner.rkt")

(test (λ (e) (run (compile (parse e)))))

