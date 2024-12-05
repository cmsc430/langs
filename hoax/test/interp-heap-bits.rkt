#lang racket
(require "test-runner.rkt")
(require "../parse.rkt")
(require "../interp-heap-bits.rkt")
(require "../interp-io.rkt")

(test (λ (e) (interp (parse e))))

;; FIXME: this is not running a heap-based interpreter!
(test/io (λ (s e) (interp/io (parse e) s)))

