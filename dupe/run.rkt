#lang racket
(require a86/interp)
(require "types.rkt")
(provide run)

;; Run instructions with run-time system linked in

;; Asm -> Value
(define (run is)
  (bits->value (asm-interp is)))

;; Run instruction and feed input from string,
;; collection output as a string (useful for testing I/O programs)


