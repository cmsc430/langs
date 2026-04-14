#lang racket
(require "test-runner.rkt"
         "../syntax/parse.rkt"
         "../interpreter/interp.rkt"
         "../interpreter/interp-io.rkt")

(test-runner    (λ p (interp (parse p))))
(test-runner-io (λ (s . p) (interp/io (parse p) s)))
