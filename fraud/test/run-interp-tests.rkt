#lang racket
(require "../interpreter/interp.rkt")
(require "../interpreter/interp-io.rkt")
(require "../syntax/parse.rkt")
(require "define-tests.rkt")
(test (λ (e) (interp (parse-closed e))))
(test/io (λ (in e) (interp/io (parse-closed e) in)))

