#lang racket
(require "../syntax/parse.rkt")
(require "../executor/exec.rkt")
(require "define-tests.rkt")
(test (λ (e) (exec (parse e))))

