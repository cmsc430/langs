#lang racket
(provide compile-literals init-symbol-table compile-string-chars symbol->data-label)
(require "../syntax/ast.rkt")
(require "../syntax/literals.rkt")
(require "../runtime/types.rkt")
(require a86/ast a86/registers)

;; Prog -> Asm
(define (compile-literals p)
  (append-map compile-literal (literals p)))

;; Symbol -> Asm
(define (compile-literal s)
  (let ((str (symbol->string s)))
    (seq (Label (symbol->data-label s))
         (Dq (value->bits (string-length str)))
         (compile-string-chars (string->list str))
         (if (odd? (string-length str))
             (seq (Dd 0))
             (seq)))))

;; Prog -> Asm
;; Call intern_symbol on every symbol in the program
(define (init-symbol-table p)
  (match (symbols p)
    ['() (seq)]
    [ss  (seq (Sub 'rsp 8)
              (append-map init-symbol ss)
              (Add 'rsp 8))]))

;; Symbol -> Asm
(define (init-symbol s)
  (seq (Lea rdi (symbol->data-label s))
       (Extern 'intern_symbol)
       (Call 'intern_symbol)))

;; [Listof Char] -> Asm
(define (compile-string-chars cs)
  (match cs
    ['() (seq)]
    [(cons c cs)
     (seq (Dd (char->integer c))
          (compile-string-chars cs))]))

(define (symbol->data-label s)
  (symbol->label
   (string->symbol (string-append "data_" (symbol->string s)))))

