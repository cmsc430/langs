#lang racket
(provide interp/io)
(require "interp.rkt")
;; String Prog -> (Cons Answer String)
;; Interpret p with given string as input,
;; return answer and collected output as string
(define (interp/io p input)
  (define result (box #f))
  (define output
    (with-input-from-string input
      (λ ()
        (with-output-to-string
          (λ ()
            (set-box! result (interp p)))))))
  (cons (unbox result) output))

