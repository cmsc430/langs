#lang racket
(provide main)
(require "../syntax/parse.rkt")
(require "../compiler/compile.rkt")
(require "run.rkt")

;; -> Void
;; Compile contents of stdin and use asm-interp to run
(define (main)
  (read-line) ; ignore #lang racket line
  (run (compile (parse (read)))))

