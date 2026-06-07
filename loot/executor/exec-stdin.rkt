#lang racket
(provide main)
(require "../syntax/parse.rkt")
(require "exec.rkt")

;; -> Value
;; Parse, compile, and execute contents of stdin
(define (main)
  (read-line) ; ignore #lang racket line
  (exec (parse (read))))

