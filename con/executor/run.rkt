#lang racket
(require a86/interp)
(require "exec.rkt")
(provide run)

;; Asm -> Integer
(define (run asm)
  (call-with-exec
   asm
   identity))

