#lang racket
(provide main)
(require "../syntax/parse.rkt"
         "../syntax/read-all.rkt"
         "interp.rkt")

;; -> Void
;; Parse and interpret contents of stdin,
;; print result on stdout
(define (main)
  (read-line) ; ignore #lang racket line
  (let ((r (interp (parse (read-all)))))
    (unless (void? r)
      (println r))))
