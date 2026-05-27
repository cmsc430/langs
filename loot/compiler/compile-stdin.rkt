#lang racket
(provide main)
(require "../syntax/parse.rkt")
(require "compile.rkt")
(require "../syntax/read-all.rkt")
(require a86/printer)

;; -> Void
;; Compile contents of stdin,
;; emit asm code on stdout
(define (main)
  (read-line) ; ignore #lang racket line
  (asm-display (compile (apply parse-closed (read-all)))))

