#lang racket
(provide main)
(require "../syntax/parse.rkt"
         "../syntax/read-all.rkt"
         "../compiler/compile.rkt"
         "run.rkt"
         a86/printer)

;; -> Void
;; Compile contents of stdin,
;; emit asm code on stdout
(define (main)
  (read-line) ; ignore #lang racket line
  (run (parse (read-all))))
