#lang racket
(require "../compile.rkt")
(require "../parse.rkt")
(require "../exec.rkt")
(require "test-runner.rkt")

(test (λ (e) (exec (parse e))))

