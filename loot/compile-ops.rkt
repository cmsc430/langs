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
    ['read-byte (seq pad-stack (Call 'read_byte) unpad-stack)]
    ['peek-byte (seq pad-stack (Call 'peek_byte) unpad-stack)]))

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
     (seq (assert-byte rax)
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
     (let ((zero (gensym))
           (done (gensym)))
       (seq (assert-vector rax)
            (Cmp rax type-vect)
            (Je zero)
            (Mov rax (Mem rax (- type-vect)))
            (Sal rax int-shift)
            (Jmp done)
            (Label zero)
            (Mov rax 0)
            (Label done)))]
    ['string-length
     (let ((zero (gensym))
           (done (gensym)))
       (seq (assert-string rax)
            (Cmp rax type-str)
            (Je zero)
            (Mov rax (Mem rax (- type-str)))
            (Sal rax int-shift)
            (Jmp done)
            (Label zero)
            (Mov rax 0)
            (Label done)))]))


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
    ['make-vector ;; size value
     (let ((loop (gensym))
           (done (gensym))
           (empty (gensym)))
       (seq (Pop r8) ;; r8 = size
            (assert-natural r8)
            (Cmp r8 0) ; special case empty vector
            (Je empty)

            (Mov r9 rbx)
            (Xor r9 type-vect)

            (Sar r8 int-shift)
            (Mov (Mem rbx) r8)
            (Add rbx 8)

            (Label loop)
            (Mov (Mem rbx) rax)
            (Add rbx 8)
            (Sub r8 1)
            (Cmp r8 0)
            (Jne loop)

            (Mov rax r9)
            (Jmp done)

            (Label empty)
            (Mov rax type-vect)
            (Label done)))]
    ['vector-ref ; vector index
     (seq (Pop r8)
          (assert-vector r8)
          (assert-integer rax)
          (Cmp r8 type-vect)
          (Je 'err) ; special case for empty vector
          (Cmp rax 0)
          (Jl 'err)
          (Xor r8 type-vect)      ; r8 = ptr
          (Mov r9 (Mem r8))       ; r9 = len
          (Sar rax int-shift)     ; rax = index
          (Sub r9 1)
          (Cmp r9 rax)
          (Jl 'err)
          (Sal rax 3)
          (Add r8 rax)
          (Mov rax (Mem r8 8)))]
    ['make-string
     (let ((loop (gensym))
           (done (gensym))
           (empty (gensym)))
       (seq (Pop r8)
            (assert-natural r8)
            (assert-char rax)
            (Cmp r8 0) ; special case empty string
            (Je empty)

            (Mov r9 rbx)
            (Xor r9 type-str)

            (Sar r8 int-shift)
            (Mov (Mem rbx) r8)
            (Add rbx 8)

            (Sar rax char-shift)

            (Add r8 1) ; adds 1
            (Sar r8 1) ; when
            (Sal r8 1) ; len is odd

            (Label loop)
            (Mov (Mem rbx) eax)
            (Add rbx 4)
            (Sub r8 1)
            (Cmp r8 0)
            (Jne loop)

            (Mov rax r9)
            (Jmp done)

            (Label empty)
            (Mov rax type-str)
            (Label done)))]
    ['string-ref
     (seq (Pop r8)
          (assert-string r8)
          (assert-integer rax)
          (Cmp r8 type-str)
          (Je 'err) ; special case for empty string
          (Cmp rax 0)
          (Jl 'err)
          (Xor r8 type-str)       ; r8 = ptr
          (Mov r9 (Mem r8))  ; r9 = len
          (Sar rax int-shift)     ; rax = index
          (Sub r9 1)
          (Cmp r9 rax)
          (Jl 'err)
          (Sal rax 2)
          (Add r8 rax)
          (Mov eax (Mem r8 8))
          (Sal rax char-shift)
          (Xor rax type-char))]))

;; Op3 -> Asm
(define (compile-op3 p)
  (match p
    ['vector-set!
     (seq (Pop r10)
          (Pop r8)
          (assert-vector r8)
          (assert-integer r10)
          (Cmp r8 type-vect)
          (Je 'err)
          (Cmp r10 0)
          (Jl 'err)
          (Xor r8 type-vect)       ; r8 = ptr
          (Mov r9 (Mem r8))        ; r9 = len
          (Sar r10 int-shift)      ; r10 = index
          (Sub r9 1)
          (Cmp r9 r10)
          (Jl 'err)
          (Sal r10 3)
          (Add r8 r10)
          (Mov (Mem r8 8) rax)
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

