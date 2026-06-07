#lang racket
(provide run exec exec/io)
(require "../compiler/compile.rkt")
(require "decode.rkt")
(require "host.rkt")
;; Asm -> Value
(define (run asm)
  (bits->value (asm-interp/host asm)))
;; Expr -> Value
(define (exec e)
  (run (compile e)))

;; Asm String -> (cons Value String)
(define (exec/io asm in)
  (parameterize ((current-output-port (open-output-string))
                 (current-input-port  (open-input-string in)))
    (cons (exec asm)
          (get-output-string (current-output-port)))))

