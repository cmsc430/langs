#lang racket
(require "../syntax/parse.rkt")
(require "../executor/exec.rkt")
(require "define-tests.rkt")
(test (λ p (exec (apply parse-closed p))))
(test/io (λ (in . p) (exec/io (apply parse-closed p) in)))

