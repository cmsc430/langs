#lang racket
(provide interp/io)
(require "interp.rkt")
;; String Expr -> (Cons Answer String)
;; Interpret e with given string as input,
;; return answer and collected output as string
(define (interp/io e input)
  (parameterize ((current-input-port (open-input-string input))
                 (current-output-port (open-output-string)))
    (cons (interp e)
          (get-output-string (current-output-port)))))

