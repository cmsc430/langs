#lang racket
(require a86/interp)
(provide run)

;; Run instructions with run-time system linked in
;; Asm -> Integer
(define (run is)
  (asm-interp is))

;; Run instruction and feed input from string,
;; collection output as a string (useful for testing I/O programs)


