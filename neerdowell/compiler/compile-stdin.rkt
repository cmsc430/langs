#lang racket
(provide main)
(require "../syntax/parse.rkt"
         "../syntax/read-all.rkt"
         "compile.rkt"
         a86/printer)

;; -> Void
;; Compile contents of stdin,
;; emit asm code on stdout
(define (main)
  (read-line) ; ignore #lang racket line
  (asm-display (compile (parse (read-all)))))
