#lang racket
(provide compile)

(require "../syntax/ast.rkt")
(require a86/ast a86/registers)

;; Expr -> Asm
(define (compile e)
  (prog (Global 'entry)
        (Label 'entry)
        (match e
          [(Lit i) (Mov rax i)])
        (Ret)))

