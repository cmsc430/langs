#lang racket
(require "../compiler/compile.rkt")
(require "../syntax/parse.rkt")
(require "../executor/run.rkt")
(require "test-runner.rkt")
(test (λ p (run (compile (apply parse-closed p)))))
(test/io (λ (in . p) (run/io (compile (apply parse-closed p)) in)))

