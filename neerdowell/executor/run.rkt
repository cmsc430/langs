#lang racket

(provide run run/io)

(require "exec.rkt"
         "decode.rkt")

;; Asm -> Value
(define (run asm)
  (call-with-exec
   asm
   (λ (r)
     (match r
       ['err 'err]
       [b (bits->value b)]))))

(define (run/io asm in)
  (parameterize ((current-output-port (open-output-string))
                 (current-input-port  (open-input-string in)))
    (cons (run asm)
          (get-output-string (current-output-port)))))
