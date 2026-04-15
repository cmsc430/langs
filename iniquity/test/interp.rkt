#lang racket
(require "../interpreter/interp.rkt")
(require "../interpreter/interp-io.rkt")
(require "../syntax/parse.rkt")
(require "test-runner.rkt")
(test (λ p (interp (apply parse-closed p))))
(test/io (λ (in . p) (interp/io (apply parse-closed p) in)))

