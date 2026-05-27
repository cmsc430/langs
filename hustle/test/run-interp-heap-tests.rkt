#lang racket
(require "define-tests.rkt")
(require "../syntax/parse.rkt")
(require "../interpreter/interp-heap.rkt")
(require "../interpreter/interp-io.rkt")

(test (λ (e) (interp (parse e))))

;; FIXME: this is not running a heap-based interpreter!
(test/io (λ (s e) (interp/io (parse e) s)))

