#lang racket
(provide compile
         compile-e)

(require "ast.rkt")
(require "compile-ops.rkt")
(require "types.rkt")
(require a86/ast a86/registers)

;; Expr -> Asm
(define (compile e)
  (prog (Global 'entry)
        (Label 'entry)
        (compile-e e)
        (Ret)))

;; Expr -> Asm
(define (compile-e e)
  (match e
    [(Lit d) (compile-datum d)]
    [(Prim1 p e) (compile-prim1 p e)]
    [(If e1 e2 e3)
     (compile-if e1 e2 e3)]))

;; Datum -> Asm
(define (compile-datum d)
  (seq (Mov rax (value->bits d))))

;; Op1 Expr -> Asm
(define (compile-prim1 p e)
  (seq (compile-e e)
       (compile-op1 p)))

;; Expr Expr Expr -> Asm
(define (compile-if e1 e2 e3)
  (let ((l1 (gensym 'if))
        (l2 (gensym 'if)))
    (seq (compile-e e1)
         (Cmp rax (value->bits #f))
         (Je l1)
         (compile-e e2)
         (Jmp l2)
         (Label l1)
         (compile-e e3)
         (Label l2))))

