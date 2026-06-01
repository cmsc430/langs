#lang racket
(provide main)
(require "../syntax/parse.rkt")
(require "../compiler/compile.rkt")
(require a86/interp)

;; -> Void
;; Compile contents of stdin and use asm-interp to run
(define (main)
  (read-line) ; ignore #lang racket line
  (asm-interp (compile (parse (read)))))

