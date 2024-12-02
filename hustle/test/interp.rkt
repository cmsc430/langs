#lang racket
(require "../interp.rkt")
(require "../interp-io.rkt")
(require "../parse.rkt")
(require "test-runner.rkt")
(test (λ (e) (interp (parse-closed e))))
(test/io (λ (in e) (interp/io (parse-closed e) in)))

