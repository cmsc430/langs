#lang racket
(require "../interpreter/interp.rkt")
(require "../interpreter/interp-io.rkt")
(require "../syntax/parse.rkt")
(require "test-runner.rkt")

(test (λ (e) (interp (parse e))))

(test/io (λ (in e) (interp/io (parse e) in)))

