#lang racket
(require "../interp.rkt")
(require "../interp-io.rkt")
(require "../parse.rkt")
(require "test-runner.rkt")
(test (λ p (interp (apply parse-closed p))))
(test/io (λ (in . p) (interp/io (apply parse-closed p) in)))

