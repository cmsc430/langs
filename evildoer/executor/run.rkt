#lang racket
(require a86/interp)
(require "decode.rkt")
(require "exec.rkt")
(provide run run/io)
;; Asm -> Value
(define (run asm)
  (call-with-exec
   asm
   (λ (r)
     (bits->value r))))

;; Asm String -> (cons Value String)
(define (run/io asm in)
  (parameterize ((current-output-port (open-output-string))
                 (current-input-port  (open-input-string in)))
    (cons (run asm)
          (get-output-string (current-output-port)))))

