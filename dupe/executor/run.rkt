#lang racket
(require a86/interp)
(require "decode.rkt")
(require "exec.rkt")
(provide run)
(define (run asm)
  (call-with-exec
   asm
   (λ (r)
     (bits->value r))))

