#lang racket
(provide run exec exec/io)
(require "../compiler/compile.rkt")
(require "decode.rkt")
(require "host.rkt")
;; Asm -> Answer
(define (run asm)
  (with-handlers ([(λ (x) (eq? x 'err)) identity])
    (bits->value (asm-interp/host asm))))
;; Expr -> Answer
(define (exec e)
  (run (compile e)))
;; Asm String -> (cons Answer String)
(define (exec/io asm in)
  (parameterize ((current-output-port (open-output-string))
                 (current-input-port  (open-input-string in)))
    (cons (exec asm)
          (get-output-string (current-output-port)))))

