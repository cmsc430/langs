#lang racket
(require "../compile.rkt")
(require "../parse.rkt")
(require "../exec.rkt")
(require "../exec-io.rkt")
(require "test-runner.rkt")

(test (λ (e) (exec (parse e))))

(test/io (λ (i e) (exec/io (parse e) i)))

