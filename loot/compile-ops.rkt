#lang racket
(provide compile-op0 compile-op1 compile-op2 compile-op3 pad-stack assert-proc)
(require "ast.rkt")
(require "types.rkt")
(require "assert.rkt")
(require a86/ast a86/registers)

;; Op0 -> Asm
(define (compile-op0 p)
  (match p
    ['void      (seq (Mov rax (value->bits (void))))]
    ['read-byte (seq (Extern 'read_byte) pad-stack (Call 'read_byte) unpad-stack)]
    ['peek-byte (seq (Extern 'peek_byte) pad-stack (Call 'peek_byte) unpad-stack)]))

;; Op1 -> Asm
(define (compile-op1 p)
  (match p
    ['add1
     (seq (assert-integer rax)
          (Add rax (value->bits 1)))]
    ['sub1
     (seq (assert-integer rax)
          (Sub rax (value->bits 1)))]
    ['zero?
     (seq (assert-integer rax)
          (Cmp rax 0)
          if-equal)]
    ['char?
     (seq (And rax mask-char)
          (Cmp rax type-char)
          if-equal)]
    ['char->integer
     (seq (assert-char rax)
          (Sar rax char-shift)
          (Sal rax int-shift))]
    ['integer->char
     (seq (assert-codepoint rax)
          (Sar rax int-shift)
          (Sal rax char-shift)
          (Xor rax type-char))]
    ['eof-object?
     (seq (Cmp rax (value->bits eof))
          if-equal)]
    ['write-byte
     (seq (Extern 'write_byte)
          (assert-byte rax)
          pad-stack
          (Mov rdi rax)
          (Call 'write_byte)
          unpad-stack)]
    ['box
     (seq (Mov (Mem rbx) rax) ; memory write
          (Mov rax rbx)            ; put box in rax
          (Xor rax type-box)       ; tag as a box
          (Add rbx 8))]
    ['unbox
     (seq (assert-box rax)
          (Mov rax (Mem rax (- type-box))))]
    ['car
     (seq (assert-cons rax)
          (Mov rax (Mem rax (- 0 type-cons))))]
    ['cdr
     (seq (assert-cons rax)
          (Mov rax (Mem rax (- 8 type-cons))))]

    ['empty? (seq (Cmp rax (value->bits '())) if-equal)]
    ['cons? (type-pred ptr-mask type-cons)]
    ['box?  (type-pred ptr-mask type-box)]
    ['vector? (type-pred ptr-mask type-vect)]
    ['string? (type-pred ptr-mask type-str)]
    ['vector-length
     (seq (assert-vector rax)
          (Mov rax (Mem rax (- type-vect))))]
    ['string-length
     (seq (assert-string rax)
          (Mov rax (Mem rax (- type-str))))]))

;; Op2 -> Asm
(define (compile-op2 p)
  (match p
    ['+
     (seq (Pop r8)
          (assert-integer r8)
          (assert-integer rax)
          (Add rax r8))]
    ['-
     (seq (Pop r8)
          (assert-integer r8)
          (assert-integer rax)
          (Sub r8 rax)
          (Mov rax r8))]
    ['<
     (seq (Pop r8)
          (assert-integer r8)
          (assert-integer rax)
          (Cmp r8 rax)
          if-lt)]
    ['=
     (seq (Pop r8)
          (assert-integer r8)
          (assert-integer rax)
          (Cmp r8 rax)
          if-equal)]
    ['cons
     (seq (Mov (Mem rbx 8) rax)
          (Pop rax)
          (Mov (Mem rbx 0) rax)
          (Mov rax rbx)
          (Xor rax type-cons)
          (Add rbx 16))]
    ['eq?
     (seq (Pop r8)
          (Cmp rax r8)
          if-equal)]
    ['make-vector
     (let ((nonzero (gensym 'nz))
           (loop (gensym 'loop))
           (theend (gensym 'theend)))

       (seq (Pop r8)
            (assert-natural r8)

            ; special case for length = 0
            (Cmp r8 0)
            (Jne nonzero)
            ; return canonical representation
            (Lea rax (Mem 'empty type-vect))
            (Jmp theend)

            ; Code for nonzero case
            (Label nonzero)
            (Mov (Mem rbx 0) r8) ; write length
            (Sar r8 1)           ; convert to bytes
            (Mov r9 r8)          ; save for heap adjustment

            ; start initialization
            (Label loop)
            (Mov (Mem rbx r8) rax)
            (Sub r8 8)
            (Cmp r8 0)
            (Jne loop)
            ; end initialization

            (Mov rax rbx)
            (Xor rax type-vect)  ; create tagged pointer
            (Add rbx r9)         ; acct for elements and stored length
            (Add rbx 8)
            (Label theend)))]

    ['vector-ref
     (seq (Pop r8)
          (assert-vector r8)
          (assert-natural rax)
          (Mov r9 (Mem r8 (- type-vect)))
          (Cmp rax r9)
          (Jge 'err)
          (Sar rax 1)
          (Mov rax (Mem r8 rax (- 8 type-vect))))]
    ['make-string
     (let ((nonzero (gensym 'nz))
           (loop (gensym 'loop))
           (theend (gensym 'theend)))
       (seq (Pop r8)
            (assert-natural r8)
            (assert-char rax)

            ; special case for length = 0
            (Cmp r8 0)
            (Jne nonzero)
            ; return canonical representation
            (Lea rax (Mem 'empty type-str))
            (Jmp theend)

            ; Code for nonzero case
            (Label nonzero)

            (Mov (Mem rbx 0) r8) ; write length
            (Sar r8 2)           ; convert to bytes
            (Mov r9 r8)          ; save for heap adjustment

            (Sar rax char-shift) ; convert to codepoint

            ; start initialization
            (Label loop)
            (Mov (Mem rbx r8 4) eax)
            (Sub r8 4)
            (Cmp r8 0)
            (Jne loop)
            ; end initialization

            (Mov rax rbx)
            (Xor rax type-str)   ; create tagged pointer
            (Add rbx r9)         ; acct for elements and stored length
            (Add rbx 8)
            ; Pad to 8-byte alignment
            (Add rbx 4)
            (Sar rbx 3)
            (Sal rbx 3)
            (Label theend)))]

    ['string-ref
     (seq (Pop r8)
          (assert-natural rax)
          (assert-string r8)
          (Mov r9 (Mem r8 (- type-str)))
          (Cmp rax r9)
          (Jge 'err)
          (Sar rax 2)
          (Mov eax (Mem r8 rax (- 8 type-str)))
          (Sal rax char-shift)
          (Xor rax type-char))]))


;; Op3 -> Asm
(define (compile-op3 p)
  (match p
    ['vector-set!
     (seq (Pop r10)
          (Pop r8)
          (assert-vector r8)
          (assert-natural r10)
          (Mov r9 (Mem r8 (- type-vect)))
          (Cmp r10 r9)
          (Jge 'err)
          (Sar r10 1) ; convert to byte offset
          (Mov (Mem r8 r10 (- 8 type-vect)) rax)
          (Mov rax (value->bits (void))))]))

(define (type-pred mask type)
  (seq (And rax mask)
       (Cmp rax type)
       if-equal))

;; Asm
;; set rax to #t or #f if comparison flag is equal
(define if-equal
  (seq (Mov rax (value->bits #f))
       (Mov r9  (value->bits #t))
       (Cmove rax r9)))

;; Asm
;; set rax to #t or #f if comparison flag is less than
(define if-lt
  (seq (Mov rax (value->bits #f))
       (Mov r9  (value->bits #t))
       (Cmovl rax r9)))


;; Asm
;; Dynamically pad the stack to be aligned for a call
(define pad-stack
  (seq (Mov r15 rsp)
       (And r15 #b1000)
       (Sub rsp r15)))

;; Asm
;; Undo the stack alignment after a call
(define unpad-stack
  (seq (Add rsp r15)))

