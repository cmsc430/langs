#lang racket
(provide exec exec/io)
(require "../compiler/compile.rkt")
(require "decode.rkt")
(require "host.rkt")

;; Expr -> Value
(define (exec e)
  (bits->value (asm-interp/host (compile e))))

;; Asm String -> (cons Value String)
(define (exec/io e in)
  (parameterize ((current-output-port (open-output-string))
                 (current-input-port  (open-input-string in)))
    (cons (exec e)
          (get-output-string (current-output-port)))))
    
