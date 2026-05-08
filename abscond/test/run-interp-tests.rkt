#lang racket
(require "../interpreter/interp.rkt")
(require "../syntax/parse.rkt")
(require "define-tests.rkt")

(test (λ (e) (interp (parse e))))

