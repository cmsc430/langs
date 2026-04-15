#lang racket
(require a86/interp)
(require "decode.rkt")
(require "exec.rkt")
(provide run run/io)
;; Asm -> Answer
(define (run asm)
  (call-with-exec
   asm
   (λ (r)
     (match r
       ['err 'err]
       [b (bits->value b)]))))
;; Asm String -> (cons Answer String)
(define (run/io asm in)
  (parameterize ((current-output-port (open-output-string))
                 (current-input-port  (open-input-string in)))
    (cons (run asm)
          (get-output-string (current-output-port)))))

