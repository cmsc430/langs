#lang racket
(require "../compile.rkt")
(require "../parse.rkt")
(require "../exec.rkt")
(require "test-runner.rkt")
(test (λ (e) (exec (parse-closed e))))
(test/io (λ (i e) (exec/io (parse-closed e) i)))

