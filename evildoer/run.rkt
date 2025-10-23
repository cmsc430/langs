#lang racket
(require a86/interp)
(require "types.rkt")
(require "build-runtime.rkt")
(provide run run/io)

;; Run instructions with run-time system linked in

;; Asm -> Value
(define (run is)
  (match (run/io is "")
    [(cons r out) (begin (display out) r)]))

;; Run instruction and feed input from string,
;; collection output as a string (useful for testing I/O programs)
;; Asm String -> (cons Value String)
(define (run/io is in)
  (parameterize ((current-objs (list (path->string runtime-path))))
    (match (asm-interp/io is in)
      [(cons b out)
       (cons (bits->value b) out)])))

