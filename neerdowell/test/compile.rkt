#lang racket
(require "test-runner.rkt"
         "../syntax/parse.rkt"
         "../compiler/compile.rkt"
         "../executor/run.rkt")

(test-runner    (λ p (run (compile (parse p)))))
(test-runner-io (λ (s . p) (run/io (compile (parse p)) s)))
