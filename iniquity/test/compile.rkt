#lang racket
(require "../compile.rkt")
(require "../parse.rkt")
(require "../exec.rkt")
(require "../exec-io.rkt")
(require "test-runner.rkt")
(test (λ p (exec (apply parse-closed p))))
(test/io (λ (in . p) (exec/io (apply parse-closed p) in)))

