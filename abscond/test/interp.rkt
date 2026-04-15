#lang racket
(require "../interpreter/interp.rkt")
(require "../syntax/parse.rkt")
(require "test-runner.rkt")

(test (λ (e) (interp (parse e))))

