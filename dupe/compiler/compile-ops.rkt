#lang racket
(provide compile-op1)
(require "../syntax/ast.rkt")
(require "../runtime/types.rkt")
(require a86/ast a86/registers)

;; Op1 -> Asm
(define (compile-op1 p)
  (match p
    ['add1 (Add rax (value->bits 1))]
    ['sub1 (Sub rax (value->bits 1))]
    ['zero?
     (seq (Cmp rax 0)
          (Mov rax (value->bits #f))
          (Mov r9  (value->bits #t))
          (Cmove rax r9))]))

