#lang racket
(provide interp/io)
(require "interp.rkt")
;; String Prog -> (Cons Answer String)
;; Interpret p with given string as input,
;; return answer and collected output as string
(define (interp/io p input)
  (parameterize ((current-input-port (open-input-string input))
                 (current-output-port (open-output-string)))
    (cons (interp p)
          (get-output-string (current-output-port)))))

