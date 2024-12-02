#lang racket
(provide interp/io)
(require "interp.rkt")

;; String Expr -> (Cons Value String)
;; Interpret e with given string as input,
;; return value and collected output as string
(define (interp/io e input)
  (define result (box #f))
  (define output
    (with-input-from-string input
      (λ ()
        (with-output-to-string
          (λ ()
            (set-box! result (interp e)))))))
  (cons (unbox result) output))

