#lang racket

;; ast.rkt
;; parse.rkt
;; types.rkt
;; fv.rkt
;; lambdas.rkt
;; a86/ast.rkt
;; utils.rkt
;; compile-ops.rkt
;; compile-datum.rkt
;; compile-expr.rkt
;; compile-define.rkt
;; compile-literals.rkt
;; compile.rkt

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; ast.rkt

(provide (all-defined-out))

;; type Prog = (Prog (Listof Defn))
(struct Prog (ds))

;; type Lib = (Lib (Listof Id) (Listof Defn))
(struct Lib (ids ds))

;; type Defn = (Defn Id Lambda)
(struct Defn (f l))

;; type Expr   = (Eof)
;;             | (Quote Datum)
;;             | (Prim Op (Listof Expr))
;;             | (If Expr Expr Expr)
;;             | (Begin Expr Expr)
;;             | (Let (Listof Id) (Listof Expr) Expr)
;;             | (Var Id)
;;             | (Match Expr (Listof Pat) (Listof Expr))
;;             | (App Expr (Listof Expr))
;;             | Lambda
;;             | (Apply Expr (Listof Expr))
;; type Lambda = (Lam Id (Listof Id) Expr)
;;             | (LamRest Id (Listof Id) Id Expr)
;;             | (LamCase Id (Listof LamCaseClause))
;; type LamCaseClause =
;;             | (Lam Id (Listof Id) Expr)
;;             | (LamRest Id (Listof Id) Expr)
;; type Datum = Integer
;;            | Char
;;            | Boolean
;;            | String
;;            | Symbol
;;            | (Boxof Datum)
;;            | (Listof Datum)
;;            | (Vectorof Datum)
;; type Id    = Symbol
;; type Op    = Op0 | Op1 | Op2 | Op3
;; type Op0   = 'read-byte
;; type Op1   = 'add1 | 'sub1 | 'zero?
;;            | 'char? | 'integer->char | 'char->integer
;;            | 'write-byte | 'eof-object?
;;            | 'box | 'car | 'cdr | 'unbox
;;            | 'empty? | 'cons? | 'box?
;;            | 'vector? | 'vector-length
;;            | 'string? | 'string-length
;;            | 'symbol? | 'symbol->string
;;            | 'string->symbol | 'string->uninterned-symbol
;; type Op2   = '+ | '- | '< | '=
;;            | 'cons
;;            | 'make-vector | 'vector-ref
;;            | 'make-string | 'string-ref
;;            | 'struct?
;; type Op3   = 'vector-set! | 'struct-ref
;; type OpN   = 'make-struct
;; type Pat   = (PVar Id)
;;            | (PWild)
;;            | (PLit Lit)
;;            | (PBox Pat)
;;            | (PCons Pat Pat)
;;            | (PAnd Pat Pat)
;;            | (PSymb Symbol)
;;            | (PStr String)
;;            | (PStruct Id (Listof Pat))
;;            | (PPred Expr)
;; type Lit   = Boolean
;;            | Character
;;            | Integer
;;            | '()

(struct Eof     ())
(struct Prim    (p es))
(struct If      (e1 e2 e3))
(struct Begin   (e1 e2))
(struct Let     (xs es e))
(struct Var     (x))
(struct App     (e es))
(struct Lam     (f xs e))
(struct LamRest (f xs x e))
(struct LamCase (f cs))
(struct Apply   (e es el))
(struct Quote   (d))
(struct Match   (e ps es))

(struct PVar  (x))
(struct PWild ())
(struct PLit  (x))
(struct PBox  (p))
(struct PCons (p1 p2))
(struct PAnd  (p1 p2))
(struct PSymb (s))
(struct PStr (s))
(struct PStruct (n ps))
(struct PPred (e))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; parse.rkt

(provide parse parse-define parse-e parse-library)
(require "ast.rkt")

;; [Listof S-Expr] -> Prog
(define (parse s)
  (let ((loaded (box '())))
    (match s
      ['() (Prog '())]
      [(cons (and (cons (? def-keyword?) _) d) '())
       (Prog (append (parse-define d)
                     (list (Defn (gensym) (parse-e '(void))))))]    
      [(cons (and (cons (? def-keyword?) _) d) s)
       (match (parse s)
         [(Prog ds)
          (Prog (append (parse-define d) ds))])]
      [(cons (cons 'provide _) s) ; ignore provides for now    
       (parse s)]
      ;; Doesn't quite work and will make parse depend on read
      #;
      [(cons (cons 'require fs) s)
       (match (parse s)
         [(Prog ds)
          (Prog (append (load-files loaded fs) ds))])]              
      [(cons e s)
        (match (parse s)
          [(Prog ds)
           (Prog (cons (Defn (gensym) (parse-e e)) ds))])]
      [_ (error "program parse error" s)])))

(define (def-keyword? x)
  (or (eq? x 'define)
      (eq? x 'struct)))

;; [Listof S-Expr] -> Lib
(define (parse-library s)
  (match s
    [(cons (cons 'provide ids)
           (cons (cons 'require _) ds))
     (Lib ids (parse-ds ds))]))
   
;; [Listof S-Expr] -> [Listof Defn]
(define (parse-ds s)
  (match s
    ['() '()]
    [(cons d ds)
     (append (parse-define d)
             (parse-ds ds))]))

;; S-Expr -> [Listof Defn]
(define (parse-define s)
  (match s
    [(list 'define (cons f xs) e)
     (match (parse-param-list xs e)
       [(Lam l xs e)
        (list (Defn f (Lam l xs e)))]
       [(LamRest l xs x e)
        (list (Defn f (LamRest l xs x e)))])]
    [(list 'define f (cons 'case-lambda cs))
     (list (Defn f (LamCase (gensym 'lamcase)
                            (parse-case-lambda-clauses cs))))]
    [(list 'define (? symbol? x) e)
     (match (parse-e e)
       [e (list (Defn x e))])]
    [(cons 'struct _)
     (parse-struct s)]     
    [_ (error "Parse defn error" s)]))

  ;; S-Expr -> [Listof Defn]
(define (parse-struct s)
  (match s
    [(list 'struct (? symbol? n) flds)
     (if (andmap symbol? flds)
         (list* (make-struct-defn-construct n flds)
                (make-struct-defn-predicate n)
                (make-struct-defn-accessors n (reverse flds)))
         (error "parse struct definition error"))]
    [_ (error "parse struct definition error")]))

;; Id [Listof Id] -> [Listof Defn]
(define (make-struct-defn-construct n flds)
  (Defn n
    (Lam (gensym 'lam)
         flds
         (Prim 'make-struct (cons (Quote n) (map Var flds))))))

;; Id -> [Listof Defn]
(define (make-struct-defn-predicate n)
  (Defn (symbol-append n '?)
    (Lam (gensym 'lam)
         (list 'x)
         (Prim 'struct? (list (Quote n) (Var 'x))))))

;; Id [Listof Id] -> [Listof Defn]
(define (make-struct-defn-accessors n flds)
  (match flds
    ['() '()]
    [(cons f flds)
     (cons (Defn (symbol-append n '- f)
             (Lam (gensym 'lam)
                  (list 'x)
                  (Prim 'struct-ref
                        (list (Quote n)
                              (Quote (length flds))
                              (Var 'x)))))
           (make-struct-defn-accessors n flds))]))

;; Symbol ... -> Symbol
(define (symbol-append . ss)
  (string->symbol
   (apply string-append (map symbol->string ss))))

;; S-Expr -> Expr
(define (parse-e s)
  (match s
    [(? self-quoting?)             (Quote (parse-datum s))]
    [(list 'quote d)               (Quote (parse-datum d))]
    ['eof                          (Eof)]
    [(? symbol?)                   (Var s)]
    ;[(list (? (op% op0) p0))       (Prim (drop-% p0) '())]
    ;[(list (? (op% op1) p1) e)     (Prim (drop-% p1) (list (parse-e e)))]
    ;[(list (? (op% op2) p2) e1 e2) (Prim (drop-% p2) (list (parse-e e1) (parse-e e2)))]
    ;[(list (? (op% op3) p3) e1 e2 e3)
    ; (Prim (drop-% p3) (list (parse-e e1) (parse-e e2) (parse-e e3)))]
    [(list 'begin e1 e2)
     (Begin (parse-e e1) (parse-e e2))]
    [(list 'if e1 e2 e3)
     (If (parse-e e1) (parse-e e2) (parse-e e3))]
    [(list 'let bs e)
     (parse-let bs (parse-e e))]
    [(cons 'match (cons e ms))
     (parse-match (parse-e e) ms)]    
    [(list 'λ xs e)
     (parse-param-list xs e)]
    [(list 'lambda xs e)
     (parse-param-list xs e)]
    [(cons 'case-lambda cs)
     (LamCase (gensym 'lamcase)
              (parse-case-lambda-clauses cs))]
    [(cons 'apply (cons e es))
     (parse-apply (parse-e e) es)]
    [(list 'cond (list 'else e)) (parse-e e)]
    [(cons 'cond (cons (list e1 e2) r))
     (If (parse-e e1)
         (parse-e e2)
         (parse-e (cons 'cond r)))]
    [(cons 'or '())
     (Quote #f)]
    [(cons 'or (cons e es))
     (let ((x (gensym 'or)))
       (Let (list x) (list (parse-e e))
            (If (Var x) (Var x) (parse-e (cons 'or es)))))]
    [(cons e es)
     (App (parse-e e) (map parse-e es))]    
    [_ (error "Parse error" s)]))

;; S-Expr Expr -> Expr
(define (parse-let bs e)
  (match bs
    ['() (Let '() '() e)]
    [(cons (list x e0) bs)
     (match (parse-let bs e)
       [(Let xs es e)
        (Let (cons x xs) (cons (parse-e e0) es) e)])]
    [_
     (error "Parse let error")]))     

;; Expr S-Expr -> Expr
(define (parse-apply e es)
  (match es
    [(list el) (Apply e '() (parse-e el))]
    [(cons e0 es)
     (match (parse-apply e es)
       [(Apply e es el)
        (Apply e (cons (parse-e e0) es) el)])]
    [_ (error "parse apply error")]))

(define (parse-match e ms)
  (match ms
    ['() (Match e '() '())]
    [(cons (list p r) ms)
     (match (parse-match e ms)
       [(Match e ps es)
        (Match e
               (cons (parse-pat p) ps)
               (cons (parse-e r) es))])]))

(define (parse-pat p)
  (match p
    [(? boolean?) (PLit p)]
    [(? integer?) (PLit p)]
    [(? char?)    (PLit p)]
    ['_           (PWild)]
    [(? symbol?)  (PVar p)]
    [(? string?)  (PStr p)]
    [(list 'quote (? symbol? s))
     (PSymb s)]
    [(list 'quote (list))
     (PLit '())]
    [(list 'box p)
     (PBox (parse-pat p))]
    [(list 'cons p1 p2)
     (PCons (parse-pat p1) (parse-pat p2))]
    [(list 'and) (PWild)]
    [(list 'and p) (parse-pat p)]
    [(cons 'and (cons p ps))
     (PAnd (parse-pat p) (parse-pat (cons 'and ps)))]
    [(cons 'list '())
     (PLit '())]
    [(cons 'list (cons p1 ps))
     (PCons (parse-pat p1)
            (parse-pat (cons 'list ps)))]
    [(list '? e)
     (PPred (parse-e e))]
    [(cons '? (cons e ps))
     (PAnd (parse-pat (list '? e))
           (parse-pat (cons 'and ps)))]
    [(cons (? symbol? n) ps)
     (PStruct n (map parse-pat ps))]))    

;; S-Expr -> [Listof LamCaseClause]
(define (parse-case-lambda-clauses cs)
  (match cs
    ['() '()]
    [(cons c cs)
     (cons (parse-case-lambda-clause c)
           (parse-case-lambda-clauses cs))]
     [_
      (error "parse case-lambda error")]))

;; S-Expr -> LamCaseClause
(define (parse-case-lambda-clause c)
  (match c
    [(list xs e)
     (parse-param-list xs e)]))

;; S-Expr S-Expr -> Lam or LamRest
(define (parse-param-list xs e)
  (match xs
    ['() (Lam (gensym 'lam) '() (parse-e e))]
    [(cons x xs)
     (match (parse-param-list xs e)
       [(Lam f xs e) (Lam f (cons x xs) e)]
       [(LamRest f xs y e) (LamRest f (cons x xs) y e)])]
    [(? symbol? xs)
     (LamRest (gensym 'lamrest) '() xs (parse-e e))]
    [_
     (error "parse parameter list error")]))

;; Datum -> Datum
(define (parse-datum d)
  (match d
    [(box d)
     (box (parse-datum d))]    
    [(cons d1 d2)
     (cons (parse-datum d1) (parse-datum d2))]
    ['() '()]
    [(? symbol? s) s]
    [(? integer? i) i]
    [(? boolean? b) b]
    [(? string? s) s]
    [(? char? c) c]
    [(? vector? v)
     (apply vector (map parse-datum (vector->list v)))]
    [_ (error "parse datum error")]))

(define (self-quoting? x)
  (or (integer? x)
      (boolean? x)
      (char? x)
      (string? x)
      (box? x)
      (vector? x)))

(define op0
  '(read-byte peek-byte void read-char peek-char))

(define op1
  '(add1 sub1 zero? char? write-byte eof-object?
         integer->char char->integer
         box unbox empty? cons? box? car cdr
         vector? vector-length string? string-length
         symbol->string string->symbol symbol?
         number->string string->uninterned-symbol
         open-input-file
         read-byte-port
         write-char
         error integer?
         eq-hash-code))
(define op2
  '(+ - < = cons eq? make-vector vector-ref make-string string-ref
      string-append set-box! quotient remainder
      bitwise-and bitwise-ior bitwise-xor arithmetic-shift
      peek-byte-port
      ))
(define op3
  '(vector-set!))

(define (op? ops)
  (λ (x)
    (and (symbol? x)
         (memq x ops))))


(define (op% ops)
  (λ (x)
    (and (symbol? x)
         (eq? #\% (string-ref (symbol->string x) 0))
         (let ((x* (drop-% x)))
           (and (memq x* ops)
                x*)))))

(define (drop-% x)
  (string->symbol  (substring (symbol->string x) 1)))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; types.rkt

(define imm-shift          3)
(define imm-mask       #b111)
(define ptr-mask       #b111)
(define type-box       #b001)
(define type-cons      #b010)
(define type-vect      #b011)
(define type-str       #b100)
(define type-proc      #b101)
(define type-symb      #b110)
(define type-struct    #b111)
(define int-shift  (+ 1 imm-shift))
(define char-shift (+ 2 imm-shift))
(define type-int      #b0000)
(define mask-int      #b1111)
(define type-char    #b01000)
(define mask-char    #b11111)
(define val-true   #b0011000)
(define val-false  #b0111000)
(define val-eof    #b1011000)
(define val-void   #b1111000)
(define val-empty #b10011000)

(define (bits->value b)
  (cond [(= type-int (bitwise-and b mask-int))
         (arithmetic-shift b (- int-shift))]
        [(= type-char (bitwise-and b mask-char))
         (integer->char (arithmetic-shift b (- char-shift)))]
        [(= b val-true)  #t]
        [(= b val-false) #f]
        [(= b val-eof)  eof]
        [(= b val-void) (void)]
        [(= b val-empty) '()]
        [else (error "invalid bits")]))

(define (imm->bits v)
  (cond [(eof-object? v) val-eof]
        [(integer? v) (arithmetic-shift v int-shift)]
        [(char? v)
         (bitwise-ior type-char
                      (arithmetic-shift (char->integer v) char-shift))]
        [(eq? v #t) val-true]
        [(eq? v #f) val-false]
        [(void? v)  val-void]
        [(empty? v) val-empty]
        [else (error "not an immediate")]))

(define (imm-bits? v)
  (zero? (bitwise-and v imm-mask)))

(define (int-bits? v)
  (zero? (bitwise-and v mask-int)))

(define (char-bits? v)
  (= type-char (bitwise-and v mask-char)))

(define (cons-bits? v)
  (zero? (bitwise-xor (bitwise-and v imm-mask) type-cons)))

(define (box-bits? v)
  (zero? (bitwise-xor (bitwise-and v imm-mask) type-box)))

(define (vect-bits? v)
  (zero? (bitwise-xor (bitwise-and v imm-mask) type-vect)))

(define (str-bits? v)
  (zero? (bitwise-xor (bitwise-and v imm-mask) type-str)))

(define (proc-bits? v)
  (zero? (bitwise-xor (bitwise-and v imm-mask) type-proc)))

(define (symb-bits? v)
  (zero? (bitwise-xor (bitwise-and v imm-mask) type-symb)))

(define (struct-bits? v)
  (zero? (bitwise-xor (bitwise-and v imm-mask) type-struct)))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; fv.rkt

(require "ast.rkt")
(provide fv fv-)

;; Expr -> [Listof Id]
;; List all of the free variables in e
(define (fv e)
  (remove-duplicates (fv* e)))

;; Expr [Listof Id] -> [Listof Id]
(define (fv- e xs)
  (remq* xs (fv e)))

(define (fv* e)  
  (match e
    [(Var x)            (list x)]
    [(Prim p es)        (append-map fv* es)]
    [(If e1 e2 e3)      (append (fv* e1) (fv* e2) (fv* e3))]
    [(Begin e1 e2)      (append (fv* e1) (fv* e2))]
    [(Let xs es e)      (append (append-map fv* es) (remq* xs (fv* e)))]    
    [(App e1 es)        (append (fv* e1) (append-map fv* es))]
    [(Lam f xs e)       (remq* xs (fv* e))]
    [(LamRest f xs x e) (remq* (cons x xs) (fv* e))]
    [(LamCase f cs)     (append-map fv* cs)]
    [(Apply e es el)    (append (fv* e) (append-map fv* es) (fv* el))]
    [(Match e ps es)    (append (fv* e) (append-map fv-clause* ps es))]
    [_                  '()]))

;; Pat Expr -> [Listof Id]
(define (fv-clause* p e)
  (remq* (bv-pat* p) (append (fv-pat* e) (fv* e))))

;; Pat -> [Listof Id]
(define (bv-pat* p)
  (match p
    [(PVar x) (list x)]
    [(PCons p1 p2) (append (bv-pat* p1) (bv-pat* p2))]
    [(PAnd p1 p2) (append (bv-pat* p1) (bv-pat* p2))]
    [(PBox p) (bv-pat* p)]
    [(PStruct n ps) (append-map bv-pat* ps)]
    [_ '()]))

;; Pat -> [Listof Id]
(define (fv-pat* p)
  (match p
    [(PBox p) (fv-pat* p)]
    [(PCons p1 p2) (append (fv-pat* p1) (fv-pat* p2))]
    [(PAnd p1 p2) (append (fv-pat* p1) (fv-pat* p2))]
    [(PStruct n ps) (append-map fv-pat* ps)]
    [(PPred e) (fv* e)]
    [_ '()]))
  

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; lambdas.rkt

(require "ast.rkt")
(provide lambdas lambdas-ds)

;; Prog -> [Listof Lam]
;; List all of the lambda expressions in p
(define (lambdas p)
  (match p
    [(Prog ds)
     (lambdas-ds ds)]))

;; Defns -> [Listof Lam]
;; List all of the lambda expressions in ds
(define (lambdas-ds ds)
  (match ds
    ['() '()]
    [(cons (Defn f l) ds)
     (append (lambdas-e l)
             (lambdas-ds ds))]))

;; Expr -> [Listof Lam]
;; List all of the lambda expressions in e
(define (lambdas-e e)
  (match e
    [(Prim p es)        (append-map lambdas-e es)]
    [(If e1 e2 e3)      (append (lambdas-e e1) (lambdas-e e2) (lambdas-e e3))]
    [(Begin e1 e2)      (append (lambdas-e e1) (lambdas-e e2))]
    [(Let xs es e)      (append (append-map lambdas-e es) (lambdas-e e))]
    [(App e1 es)        (append (lambdas-e e1) (append-map lambdas-e es))]
    [(Lam f xs e1)       (cons e (lambdas-e e1))]
    [(LamRest f xs x e1) (cons e (lambdas-e e1))]
    [(LamCase f cs)      (cons e (lambdas-cs cs))]
    [(Apply e es el)     (append (lambdas-e e) (append-map lambdas-e es) (lambdas-e el))]
    [(Match e ps es)    (append (lambdas-e e)
                                (append-map lambdas-pat ps)
                                (append-map lambdas-e es))]
    [_                  '()]))

;; [Listof LamCaseClause] -> [Listof Lam]
(define (lambdas-cs cs)
  (match cs
    ['() '()]
    [(cons (Lam f xs e) cs)
     (append (lambdas-e e)
             (lambdas-cs cs))]
    [(cons (LamRest f xs x e) cs)
     (append (lambdas-e e)
             (lambdas-cs cs))]))

;; Pat -> [Listof Lam]
(define (lambdas-pat p)
  (match p
    [(PBox p) (lambdas-pat p)]
    [(PCons p1 p2) (append (lambdas-pat p1) (lambdas-pat p2))]
    [(PAnd p1 p2) (append (lambdas-pat p1) (lambdas-pat p2))]
    [(PStruct n ps) (append-map lambdas-pat ps)]
    [(PPred e) (lambdas-e e)]
    [_ '()]))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; a86/ast.rkt

(struct Text   ())
(struct Data   ())

(struct Global (x))
(struct Label  (x))
(struct Call   (x))
(struct Ret    ())
(struct Mov    (dst src))
(struct Add    (dst src))
(struct Sub    (dst src))
(struct Cmp    (a1 a2))
(struct Jmp    (x))
(struct Je     (x))
(struct Jne    (x))
(struct Jl     (x))
(struct Jle    (x))
(struct Jg     (x))
(struct Jge    (x))
(struct And    (dst src))
(struct Or     (dst src))
(struct Xor    (dst src))
(struct Sal    (dst i))
(struct Sar    (dst i))
(struct Push   (a1))
(struct Pop    (a1))
(struct Lea    (dst x))
(struct Div    (den))

(struct Offset (r i))
(struct Extern (x))

(struct Equ    (x v))
(struct Const  (x))
(struct Dd (x))
(struct Dq (x))
(struct Plus (e1 e2))

;; (U Instruction Asm) ... -> Asm
;; Convenient for sequencing instructions or groups of instructions
(define (seq . xs)
  (foldr (λ (x is)
           (if (list? x)
               (append x is)
               (cons x is)))
         '()
         xs))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; utils.rkt

;; (provide symbol->label symbol->data-label lookup pad-stack unpad-stack)
;; (require a86/ast)

(define rsp 'rsp)
(define r15 'r15)

;; Symbol -> Label
;; Produce a symbol that is a valid Nasm label
(define (symbol->label s)
  (to-label "label_" s))

(define (symbol->data-label s)
  (to-label "data_" s))

(define (to-label prefix s)
  (string->symbol
   (string-append
    prefix
    (list->string
     (map (λ (c)
            (if (or (char<=? #\a c #\z)
                    (char<=? #\A c #\Z)
                    (char<=? #\0 c #\9)
                    (memq c '(#\_ #\$ #\# #\@ #\~ #\. #\?)))
                c
                #\_))
         (string->list (symbol->string s))))
    "_"
    (number->string (eq-hash-code s) 16))))

;; Id CEnv -> [Maybe Integer]
(define (lookup x cenv)
  (match cenv
    ['() #f]
    [(cons y rest)
     (match (eq? x y)
       [#t 0]
       [#f (match (lookup x rest)
             [#f #f]
             [i (+ 8 i)])])]))

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

(define (*8 n)
  (arithmetic-shift n 3))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; compile-ops.rkt

;; (provide (all-defined-out))
;; (require "ast.rkt" "types.rkt" "utils.rkt" a86/ast)

(define rax 'rax) ; return
(define eax 'eax) ; 32-bit load/store
(define rbx 'rbx) ; heap
(define rdi 'rdi) ; arg1
(define rsi 'rsi) ; arg2
(define rdx 'rdx) ; arg3
(define r8  'r8)  ; scratch
(define r9  'r9)  ; scratch
(define r10 'r10) ; scratch
(define r12 'r12) ; save across call to memcpy
;; (define r15 'r15) ; stack pad (non-volatile)
;; (define rsp 'rsp) ; stack

;; Op -> Asm
(define (compile-op p)
  (match p
    ;; Op0
    ['void      (seq (Mov rax val-void))]
    ['read-byte (seq pad-stack
                     (Call 'read_byte)
                     unpad-stack)]
    ['peek-byte (seq pad-stack
                     (Call 'peek_byte)
                     unpad-stack)]

    ;; Op1
    ['add1
     (seq (assert-integer rax)
          (Add rax (imm->bits 1)))]
    ['sub1
     (seq (assert-integer rax)
          (Sub rax (imm->bits 1)))]
    ['zero?
     (seq (assert-integer rax)
          (eq-imm 0))]
    ['char?
     (type-pred mask-char type-char)]
    ['char->integer
     (seq (assert-char rax)
          (Sar rax char-shift)
          (Sal rax int-shift))]
    ['integer->char
     (seq (assert-codepoint rax)
          (Sar rax int-shift)
          (Sal rax char-shift)
          (Xor rax type-char))]
    ['eof-object? (eq-imm eof)]
    ['write-byte
     (seq (assert-byte rax)
          pad-stack
          (Mov rdi rax)
          (Call 'write_byte)
          unpad-stack
          (Mov rax val-void))]
    ['box
     (seq (Mov (Offset rbx 0) rax)
          (Mov rax rbx)
          (Or rax type-box)
          (Add rbx 8))]
    ['unbox
     (seq (assert-box rax)
          (Xor rax type-box)
          (Mov rax (Offset rax 0)))]
    ['car
     (seq (assert-cons rax)
          (Xor rax type-cons)
          (Mov rax (Offset rax 8)))]
    ['cdr
     (seq (assert-cons rax)
          (Xor rax type-cons)
          (Mov rax (Offset rax 0)))]
    ['empty? (eq-imm '())]
    ['box?
     (type-pred ptr-mask type-box)]
    ['cons?
     (type-pred ptr-mask type-cons)]
    ['vector?
     (type-pred ptr-mask type-vect)]
    ['string?
     (type-pred ptr-mask type-str)]
    ['symbol?
     (type-pred ptr-mask type-symb)]
    ['vector-length
     (let ((zero (gensym))
           (done (gensym)))
       (seq (assert-vector rax)
            (Xor rax type-vect)
            (Cmp rax 0)
            (Je zero)
            (Mov rax (Offset rax 0))
            (Sal rax int-shift)
            (Jmp done)
            (Label zero)
            (Mov rax 0)
            (Label done)))]
    ['string-length
     (let ((zero (gensym))
           (done (gensym)))
       (seq (assert-string rax)
            (Xor rax type-str)
            (Cmp rax 0)
            (Je zero)
            (Mov rax (Offset rax 0))
            (Sal rax int-shift)
            (Jmp done)
            (Label zero)
            (Mov rax 0)
            (Label done)))]
    ['string->symbol
     (seq (assert-string rax)
          (Xor rax type-str)
          (Mov rdi rax)
          pad-stack
          (Call 'intern_symbol)
          unpad-stack
          (Or rax type-symb))]
    ['symbol->string
     (seq (assert-symbol rax)
          (Xor rax type-symb)
          char-array-copy
          (Or rax type-str))]
    ['string->uninterned-symbol
     (seq (assert-string rax)
          (Xor rax type-str)
          char-array-copy
          (Or rax type-symb))]
    ['open-input-file
     (seq (assert-string rax)
          (Mov rdi rax)
          pad-stack
          (Call 'open_input_file)
          unpad-stack)]
    ['read-byte-port
     (seq (Mov rdi rax) ; assert port
          pad-stack
          (Call 'read_byte_port)
          unpad-stack)]
    ['error
     (seq (assert-string rax)
          (Mov rdi rax)
          pad-stack
          (Call 'raise_error))]
    ['integer?
     (type-pred mask-int type-int)]
    ['eq-hash-code
     (seq (Sal rax int-shift))]
    
    ;; Op2
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
          (Mov rax val-true)
          (let ((true (gensym)))
            (seq (Jl true)
                 (Mov rax val-false)
                 (Label true))))]
    ['=
     (seq (Pop r8)
          (assert-integer r8)
          (assert-integer rax)
          (Cmp r8 rax)
          (Mov rax val-true)
          (let ((true (gensym)))
            (seq (Je true)
                 (Mov rax val-false)
                 (Label true))))]
    ['cons
     (seq (Mov (Offset rbx 0) rax)
          (Pop rax)
          (Mov (Offset rbx 8) rax)
          (Mov rax rbx)
          (Or rax type-cons)
          (Add rbx 16))]
    ['eq?
     (seq (Pop r8)
          (eq r8 rax))]
    ['make-vector     
     (let ((loop (gensym))
           (done (gensym))
           (empty (gensym)))
       (seq (Pop r8)
            (assert-natural r8)
            (Cmp r8 0) ; special case empty vector
            (Je empty)

            (Mov r9 rbx)
            (Or r9 type-vect)

            (Sar r8 int-shift)
            (Mov (Offset rbx 0) r8)
            (Add rbx 8)

            (Label loop)
            (Mov (Offset rbx 0) rax)
            (Add rbx 8)
            (Sub r8 1)
            (Cmp r8 0)
            (Jne loop)

            (Mov rax r9)
            (Jmp done)

            (Label empty)
            (Mov rax type-vect)
            (Label done)))]

    ['vector-ref
     (seq (Pop r8)
          (assert-vector r8)
          (assert-integer rax)
          (Cmp rax 0)
          (Jl 'raise_error_align)
          (Xor r8 type-vect)      ; r8 = ptr
          (Mov r9 (Offset r8 0))  ; r9 = len
          (Sar rax int-shift)     ; rax = index
          (Sub r9 1)
          (Cmp r9 rax)
          (Jl 'raise_error_align)
          (Sal rax 3)
          (Add r8 rax)
          (Mov rax (Offset r8 8)))]

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
            (Or r9 type-str)

            (Sar r8 int-shift)
            (Mov (Offset rbx 0) r8)
            (Add rbx 8)

            (Sar rax char-shift)

            (Add r8 1) ; adds 1
            (Sar r8 1) ; when
            (Sal r8 1) ; len is odd

            (Label loop)
            (Mov (Offset rbx 0) eax)
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
          (Cmp rax 0)
          (Jl 'raise_error_align)
          (Xor r8 type-str)       ; r8 = ptr
          (Mov r9 (Offset r8 0))  ; r9 = len
          (Sar rax int-shift)     ; rax = index
          (Sub r9 1)
          (Cmp r9 rax)
          (Jl 'raise_error_align)
          (Sal rax 2)
          (Add r8 rax)
          (Mov 'eax (Offset r8 8))
          (Sal rax char-shift)
          (Or rax type-char))]

    ['string-append
     (seq (Pop r8)
          (assert-string r8)
          (assert-string rax)
          (Xor r8 type-str)
          (Xor rax type-str)
          (Mov 'rdi r8)
          (Mov 'rsi rax)
          (Mov rdx rbx)
          pad-stack
          (Call 'string_append)
          unpad-stack
          (Mov r8 rax)
          (Cmp r8 0)
          (let ((empty (gensym))
                (done (gensym)))
            (seq  (Je empty)          
                  (Sal r8 2)
                  (Mov rax rbx)
                  (Or rax type-str)
                  (Add rbx r8)
                  (Jmp done)
                  (Label empty)
                  (Mov rax type-str)
                  (Label done))))]

    ['struct?
     (let ((f (gensym))
           (t (gensym)))
       (seq (Pop r8)
            ; (assert-symbol r8) ; don't need to do this we generated the code
            (Mov r9 rax)
            (And r9 ptr-mask)
            (Cmp r9 type-struct)
            (Jne f)
            (Xor rax type-struct)
            (Mov rax (Offset rax 0))
            (Cmp r8 rax)
            (Mov rax (imm->bits #t))
            (Jne f)
            (Jmp t)
            (Label f)
            (Mov rax (imm->bits #f))
            (Label t)))]
    ['set-box!
     (seq (Pop r8)
          (assert-box r8)
          (Xor r8 type-box)
          (Mov (Offset r8 0) rax)
          (Mov rax val-void))]
    ['quotient
     (seq (Pop r8)
          (assert-integer r8)
          (Mov r10 rax)
          (assert-integer r10)

          (Mov rdx 0)
          (Mov rax r8)
          (Sar rax int-shift)
          (Sar r10 int-shift)
          (Div r10)
          (Sal rax int-shift))]
    ['remainder
     (seq (Pop r8)
          (assert-integer r8)
          (Mov r10 rax)
          (assert-integer r10)

          (Mov rdx 0)
          (Mov rax r8)
          (Sar rax int-shift)
          (Sar r10 int-shift)
          (Div r10)
          (Mov rax rdx)
          (Sal rax int-shift))]
    ['bitwise-and
     (seq (Pop r8)
          (assert-integer r8)
          (assert-integer rax)
          (And rax r8))]
    ['bitwise-ior
     (seq (Pop r8)
          (assert-integer r8)
          (assert-integer rax)
          (Or rax r8))]    
    ['bitwise-xor
     (seq (Pop r8)
          (assert-integer r8)
          (assert-integer rax)
          (Xor rax r8)
          (Or rax type-int))]    
    ['arithmetic-shift
     (seq (Pop r8)
          (assert-integer r8)
          (assert-integer rax)
          (Sar rax int-shift)
          (Mov 'rcx rax)
          (Sal r8 'cl)
          (Mov rax r8))]
    
    ;; Op3
    ['vector-set!
     (seq (Pop r10)
          (Pop r8)
          (assert-vector r8)
          (assert-integer r10)
          (Cmp r10 0)
          (Jl 'raise_error_align)
          (Xor r8 type-vect)       ; r8 = ptr
          (Mov r9 (Offset r8 0))   ; r9 = len
          (Sar r10 int-shift)      ; r10 = index
          (Sub r9 1)
          (Cmp r9 r10)
          (Jl 'raise_error_align)
          (Sal r10 3)
          (Add r8 r10)
          (Mov (Offset r8 8) rax)
          (Mov rax val-void))]

    ['peek-byte-port
     (seq (Pop r8) ; assert port
          (Mov rdi r8)
          (assert-integer rax)
          (Mov rsi rax)
          pad-stack
          (Call 'peek_byte_port)
          unpad-stack)]
    
    ['struct-ref ; symbol, int, struct
     (seq (Pop r8)
          (Pop 'r11)
          (assert-struct rax)
          ;(assert-integer r8)
          (Xor rax type-struct)
          (Mov r10 (Offset rax 0))
          (Cmp 'r11 r10)
          (Jne 'raise_error_align)
          (Sar r8 int-shift)
          (Add r8 1)
          (Sal r8 3)
          (Add rax r8)
          (Mov rax (Offset rax 0)))]))

;; Nat -> Asm
;; Emit instructions for creating a structure of length n
;; using values on top of stack
(define (compile-make-struct n)
  (seq (compile-make-struct/a n 1)
       (Mov rax rbx)
       (Or rax type-struct)
       (Add rbx (*8 n))))

;; Nat Nat -> Asm
;; Pop elements off stack, writing them to heap
(define (compile-make-struct/a n i)
  (if (= n i)
      (seq (Mov (Offset rbx (*8 (- n i))) rax))
      (seq (Mov (Offset rbx (*8 (- n i))) rax)
           (Pop rax)
           (compile-make-struct/a n (add1 i)))))

;; Asm
;; Copy sized array of characters pointed to by rax
(define char-array-copy
  (seq (Mov rdi rbx)            ; dst
       (Mov rsi rax)            ; src
       (Mov rdx (Offset rax 0)) ; len
       (Add rdx 1)              ; #words = 1 + (len+1)/2
       (Sar rdx 1)
       (Add rdx 1)
       (Sal rdx 3)              ; #bytes = 8*#words
       (Mov r12 rdx)            ; save rdx before destroyed
       pad-stack
       (Call 'memcpy)
       unpad-stack
       ; rbx should be preserved by memcpy
       ;(Mov rbx rax) ; dst is returned, install as heap pointer
       (Add rbx r12)))                   

(define (assert-type mask type)
  (λ (arg)
    (seq (Mov r9 arg)
         (And r9 mask)
         (Cmp r9 type)
         (Jne 'raise_error_align))))

(define (type-pred mask type)
  (let ((l (gensym)))
    (seq (And rax mask)
         (Cmp rax type)
         (Mov rax (imm->bits #t))
         (Je l)
         (Mov rax (imm->bits #f))
         (Label l))))

(define assert-integer
  (assert-type mask-int type-int))
(define assert-char
  (assert-type mask-char type-char))
(define assert-box
  (assert-type ptr-mask type-box))
(define assert-cons
  (assert-type ptr-mask type-cons))
(define assert-vector
  (assert-type ptr-mask type-vect))
(define assert-string
  (assert-type ptr-mask type-str))
(define assert-symbol
  (assert-type ptr-mask type-symb))
(define assert-proc
  (assert-type ptr-mask type-proc))
(define assert-struct
  (assert-type ptr-mask type-struct))

(define (assert-codepoint r)
  (let ((ok (gensym)))
    (seq (assert-integer r)
         (Cmp r (imm->bits 0))
         (Jl 'raise_error_align)
         (Cmp r (imm->bits 1114111))
         (Jg 'raise_error_align)
         (Cmp r (imm->bits 55295))
         (Jl ok)
         (Cmp r (imm->bits 57344))
         (Jg ok)
         (Jmp 'raise_error_align)
         (Label ok))))

(define (assert-byte r)
  (seq (assert-integer r)
       (Cmp r (imm->bits 0))
       (Jl 'raise_error_align)
       (Cmp r (imm->bits 255))
       (Jg 'raise_error_align)))

(define (assert-natural r)
  (seq (assert-integer r)
       (Cmp r (imm->bits 0))
       (Jl 'raise_error_align)))

;; Value -> Asm
(define (eq-imm imm)
  (let ((l1 (gensym)))
    (seq (Cmp rax (imm->bits imm))
         (Mov rax val-true)
         (Je l1)
         (Mov rax val-false)
         (Label l1))))

(define (eq ir1 ir2)
  (let ((l1 (gensym)))
    (seq (Cmp ir1 ir2)
         (Mov rax val-true)
         (Je l1)
         (Mov rax val-false)
         (Label l1))))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; compile-datum.rkt

;; (provide compile-datum)
;; (require "types.rkt"
;;         "utils.rkt"
;;         a86/ast)

;; Registers used
;; (define rax 'rax) ; return

;; Datum -> Asm
(define (compile-datum d)
  (cond
    [(string? d)   (seq (Lea rax (load-string d)))]
    [(symbol? d)   (seq (Lea rax (load-symbol d)))]
    [(compound? d) (compile-compound-datum d)]
    [else          (compile-atom d)]))

(define (load-symbol s)
  (Plus (symbol->data-label s) type-symb))

(define (load-string s)
  (Plus (symbol->data-label (string->symbol s)) type-str))

;; Value -> Asm
(define (compile-atom v)
  (seq (Mov rax (imm->bits v))))

;; Datum -> Boolean
(define (compound? d)
  (or (box? d)
      (cons? d)
      (vector? d)))

;; Datum -> Asm
(define (compile-compound-datum d)
  (match (compile-quoted d)
    [(cons l is)
     (seq (Data)
          is
          (Text)
          (Lea rax l))]))

;; Datum -> (cons AsmExpr Asm)
(define (compile-quoted c)
  (cond
    [(vector? c) (compile-datum-vector (vector->list c))]
    [(box? c)    (compile-datum-box (unbox c))]
    [(cons? c)   (compile-datum-cons (car c) (cdr c))]
    [(symbol? c) (cons (load-symbol c) '())]
    [(string? c) (cons (load-string c) '())]
    [else        (cons (imm->bits c) '())]))

;; Datum -> (cons AsmExpr Asm)
(define (compile-datum-box c)
  (match (compile-quoted c)
    [(cons l1 is1)
     (let ((l (gensym 'box)))
       (cons (Plus l type-box)
             (seq (Label l)
                  (Dq l1)
                  is1)))]))

;; Datum Datum -> (cons AsmExpr Asm)
(define (compile-datum-cons c1 c2)
  (match (compile-quoted c1)
    [(cons l1 is1)
     (match (compile-quoted c2)
       [(cons l2 is2)
        (let ((l (gensym 'cons)))
          (cons (Plus l type-cons)
                (seq (Label l)
                     (Dq l2)
                     (Dq l1)
                     is1
                     is2)))])]))

;; [Listof Datum] -> (cons AsmExpr Asm)
(define (compile-datum-vector ds)
  (match ds
    ['() (cons type-vect '())]
    [_
     (let ((l (gensym 'vector))
           (cds (map compile-quoted ds)))
       (cons (Plus l type-vect)
             (seq (Label l)
                  (Dq (length ds))
                  (map (λ (cd) (Dq (car cd))) cds)
                  (append-map cdr cds))))]))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; compile-expr.rkt

;; (provide (all-defined-out))
;; (require "ast.rkt"
;;         "types.rkt"
;;         "lambdas.rkt"
;;         "fv.rkt"
;;         "utils.rkt"
;;         "compile-ops.rkt"
;;         "compile-datum.rkt"
;;         a86/ast)

;; Registers used
;; (define rax 'rax) ; return
;; (define rbx 'rbx) ; heap
;; (define rsp 'rsp) ; stack
;; (define rdi 'rdi) ; arg

;; Expr CEnv GEnv Bool -> Asm
(define (compile-e e c g t?)
  (match e
    [(Quote d)          (compile-datum d)]
    [(Eof)              (seq (Mov rax (imm->bits eof)))]
    [(Var x)            (compile-variable x c g)]
    [(Prim p es)        (compile-prim p es c g)]
    [(If e1 e2 e3)      (compile-if e1 e2 e3 c g t?)]
    [(Begin e1 e2)      (compile-begin e1 e2 c g t?)]
    [(Let xs es e)      (compile-let xs es e c g t?)]
    [(App e es)         (compile-app e es c g t?)]
    [(Apply e es el)    (compile-apply e es el c g t?)]
    [(Lam _ _ _)        (compile-lam e c g)]
    [(LamRest _ _ _ _)  (compile-lam e c g)]
    [(LamCase _ _)      (compile-lam e c g)]
    [(Match e ps es)    (compile-match e ps es c g t?)]))

;; Id CEnv GEnv -> Asm
(define (compile-variable x c g)
  (match (lookup x c)
    [#f (if (memq x g)
            (seq (Mov rax (Offset (symbol->label x) 0)))
            (error "unbound variable" x))]
    [i  (seq (Mov rax (Offset rsp i)))]))

;; Op (Listof Expr) CEnv GEnv -> Asm
(define (compile-prim p es c g)
  (seq (compile-es* es c g)
       (match p
         ['make-struct (compile-make-struct (length es))]
         [_ (compile-op p)])))

;; Expr Expr Expr CEnv GEnv Bool -> Asm
(define (compile-if e1 e2 e3 c g t?)
  (let ((l1 (gensym 'if))
        (l2 (gensym 'if)))
    (seq (compile-e e1 c g #f)
         (Cmp rax val-false)
         (Je l1)
         (compile-e e2 c g t?)
         (Jmp l2)
         (Label l1)
         (compile-e e3 c g t?)
         (Label l2))))

;; Expr Expr CEnv GEnv Bool -> Asm
(define (compile-begin e1 e2 c g t?)
  (seq (compile-e e1 c g #f)
       (compile-e e2 c g t?)))

;; [Listof Id] [Listof Expr] Expr CEnv GEnv Bool -> Asm
(define (compile-let xs es e c g t?)
  (seq (compile-es es c g)
       (compile-e e (append (reverse xs) c) g t?)
       (Add rsp (*8 (length xs)))))

;; Id [Listof Expr] CEnv GEnv Bool -> Asm
(define (compile-app f es c g t?)
  (compile-app-nontail f es c g)
  #;
  (if t?
      (compile-app-tail f es c)
      (compile-app-nontail f es c)))

;; Expr [Listof Expr] CEnv GEnv -> Asm
(define (compile-app-tail e es c g)
  (seq (compile-es (cons e es) c g)
       (move-args (add1 (length es)) (length c))
       (Add rsp (*8 (length c)))
       (Mov rax (Offset rsp (*8 (length es))))
       (assert-proc rax)
       (Xor rax type-proc)
       (Mov rax (Offset rax 0))
       (Jmp rax)))

;; Integer Integer -> Asm
(define (move-args i off)
  (cond [(zero? off) (seq)]
        [(zero? i)   (seq)]
        [else
         (seq (Mov r8 (Offset rsp (*8 (sub1 i))))
              (Mov (Offset rsp (*8 (+ off (sub1 i)))) r8)
              (move-args (sub1 i) off))]))

;; Expr [Listof Expr] CEnv GEnv -> Asm
;; The return address is placed above the arguments, so callee pops
;; arguments and return address is next frame
(define (compile-app-nontail e es c g)
  (let ((r (gensym 'ret))
        (i (*8 (length es))))
    (seq (Lea rax r)
         (Push rax)
         (compile-es (cons e es) (cons #f c) g)
         (Mov rax (Offset rsp i))
         (assert-proc rax)
         (Xor rax type-proc)
         (Mov r15 (length es))
         (Mov rax (Offset rax 0)) ; fetch the code label
         (Jmp rax)
         (Label r))))

;; Expr [Listof Expr] Expr CEnv GEnv Boolean -> Asm
(define (compile-apply e es el c g t?)
  ;; FIXME: should have tail recursive version too
  (let ((r (gensym 'ret)))
    (seq (Lea rax r)
         (Push rax)
         (compile-es (cons e es) (cons #f c) g)
         (compile-e el (append (make-list (add1 (length es)) #f) (cons #f c)) g #f)

         (Mov r10 (Offset rsp (*8 (length es))))
         
         (Mov r15 (length es))
         (let ((loop (gensym))
               (done (gensym)))
           (seq (Label loop)
                (Cmp rax val-empty)
                (Je done)
                (assert-cons rax)
                (Add r15 1)
                (Xor rax type-cons)
                (Mov r9 (Offset rax 8))
                (Push r9)
                (Mov rax (Offset rax 0))
                (Jmp loop)
                (Label done)))


         (assert-proc r10)
         (Xor r10 type-proc)
         (Mov r10 (Offset r10 0))
         
         (Jmp r10)
         (Label r))))

;; Lambda CEnv GEnv -> Asm
(define (compile-lam l c g)
  (let ((fvs (fv- l g)))
    (seq (Lea rax (symbol->label (lambda-name l)))
         (Mov (Offset rbx 0) rax)
         (free-vars-to-heap fvs c 8)
         (Mov rax rbx) ; return value
         (Or rax type-proc)         
         (Add rbx (*8 (add1 (length fvs)))))))

;; Lambda -> Id
(define (lambda-name l)
  (match l
    [(Lam f _ _) f]
    [(LamRest f _ _ _) f]
    [(LamCase f _) f]))

;; [Listof Id] CEnv Int -> Asm
;; Copy the values of given free variables into the heap at given offset
(define (free-vars-to-heap fvs c off)
  (match fvs
    ['() (seq)]
    [(cons x fvs)
     (match (lookup x c)
       [#f (error "unbound variable" x)]
       [i
        (seq (Mov r8 (Offset rsp i))
             (Mov (Offset rbx off) r8)
             (free-vars-to-heap fvs c (+ off 8)))])]))

;; [Listof Id] Int -> Asm
;; Copy the closure environment at given offset to stack
(define (copy-env-to-stack fvs off)
  (match fvs
    ['() (seq)]
    [(cons _ fvs)
     (seq (Mov r9 (Offset rax off))
          (Push r9)
          (copy-env-to-stack fvs (+ 8 off)))]))

;; [Listof Expr] CEnv GEnv -> Asm
(define (compile-es es c g)
  (match es
    ['() '()]
    [(cons e es)
     (seq (compile-e e c g #f)
          (Push rax)
          (compile-es es (cons #f c) g))]))

;; [Listof Expr] CEnv GEnv -> Asm
;; Like compile-es, but leave last subexpression in rax (if exists)
(define (compile-es* es c g)
  (match es
    ['() '()]
    [(cons e '())
     (compile-e e c g #f)]
    [(cons e es)
     (seq (compile-e e c g #f)
          (Push rax)
          (compile-es* es (cons #f c) g))]))

;; Expr [Listof Pat] [Listof Expr] CEnv GEnv Bool -> Asm
(define (compile-match e ps es c g t?)
  (let ((done (gensym)))
    (seq (compile-e e c g #f)
         (Push rax) ; save away to be restored by each clause
         (compile-match-clauses ps es (cons #f c) g done t?)
         (Jmp 'raise_error_align)
         (Label done)
         (Add rsp 8)))) ; pop the saved value being matched

;; [Listof Pat] [Listof Expr] CEnv GEnv Symbol Bool -> Asm
(define (compile-match-clauses ps es c g done t?)
  (match (cons ps es)
    [(cons '() '()) (seq)]
    [(cons (cons p ps) (cons e es))
     (seq (compile-match-clause p e c g done t?)
          (compile-match-clauses ps es c g done t?))]))

;; Pat Expr CEnv GEnv Symbol Bool -> Asm
(define (compile-match-clause p e c g done t?)
  (let ((next (gensym)))
    (match (compile-pattern p '() next)
      [(list i f cm)
       (seq (Mov rax (Offset rsp 0)) ; restore value being matched
            i
            (compile-e e (append cm c) g t?)
            (Add rsp (*8 (length cm)))
            (Jmp done)
            f
            (Label next))])))

;; Pat CEnv Symbol -> (list Asm Asm CEnv)
(define (compile-pattern p cm next)
  (match p
    [(PWild)
     (list (seq) (seq) cm)]
    [(PVar x)
     (list (seq (Push rax))
           (seq)
           (cons x cm))]
    [(PStr s)
     (let ((fail (gensym)))
       (list (seq (Lea rdi (symbol->data-label (string->symbol s)))
                  (Mov r8 rax)
                  (And r8 ptr-mask)
                  (Cmp r8 type-str)
                  (Jne fail)
                  (Xor rax type-str)
                  (Mov rsi rax)
                  pad-stack
                  (Call 'symb_cmp)
                  unpad-stack
                  (Cmp rax 0)
                  (Jne fail))
             (seq (Label fail)
                  (Add rsp (*8 (length cm)))
                  (Jmp next))
             cm))]
    [(PSymb s)
     (let ((fail (gensym)))
       (list (seq (Lea r9 (Plus (symbol->data-label s) type-symb))
                  (Cmp rax r9)
                  (Jne fail))
             (seq (Label fail)
                  (Add rsp (*8 (length cm)))
                  (Jmp next))
             cm))]
    [(PLit l)
     (let ((fail (gensym)))
       (list (seq (Cmp rax (imm->bits l))
                  (Jne fail))
             (seq (Label fail)
                  (Add rsp (*8 (length cm)))
                  (Jmp next))
             cm))]
    [(PAnd p1 p2)
     (match (compile-pattern p1 (cons #f cm) next)
       [(list i1 f1 cm1)
        (match (compile-pattern p2 cm1 next)
          [(list i2 f2 cm2)
           (list
            (seq (Push rax)
                 i1
                 (Mov rax (Offset rsp (*8 (- (sub1 (length cm1)) (length cm)))))
                 i2)
            (seq f1 f2)
            cm2)])])]
    [(PBox p)
     (match (compile-pattern p cm next)
       [(list i1 f1 cm1)
        (let ((fail (gensym)))
          (list
           (seq (Mov r8 rax)
                (And r8 ptr-mask)
                (Cmp r8 type-box)
                (Jne fail)
                (Xor rax type-box)
                (Mov rax (Offset rax 0))
                i1)
           (seq f1
                (Label fail)
                (Add rsp (*8 (length cm))) ; haven't pushed anything yet
                (Jmp next))
           cm1))])]
    [(PCons p1 p2)
     (match (compile-pattern p1 (cons #f cm) next)
       [(list i1 f1 cm1)
        (match (compile-pattern p2 cm1 next)
          [(list i2 f2 cm2)
           (let ((fail (gensym)))
             (list
              (seq (Mov r8 rax)
                   (And r8 ptr-mask)
                   (Cmp r8 type-cons)
                   (Jne fail)
                   (Xor rax type-cons)
                   (Mov r8 (Offset rax 0))
                   (Push r8)                ; push cdr
                   (Mov rax (Offset rax 8)) ; mov rax car
                   i1
                   (Mov rax (Offset rsp (*8 (- (sub1 (length cm1)) (length cm)))))
                   i2)
              (seq f1
                   f2
                   (Label fail)
                   (Add rsp (*8 (length cm))) ; haven't pushed anything yet
                   (Jmp next))
              cm2))])])]
    [(PStruct n ps)
     (match (compile-struct-patterns ps (cons #f cm) next 1)
       [(list i f cm)
        (let ((fail (gensym)))
          (list
           (seq (Mov r8 rax)
                (And r8 ptr-mask)
                (Cmp r8 type-struct)
                (Jne fail)
                (Xor rax type-struct)
                (Mov r8 (Offset rax 0))
                (Lea r9 (Plus (symbol->data-label n) type-symb))
                (Cmp r8 r9)
                (Jne fail)
                (Push rax)
                i)
           (seq f
                (Label fail)
                (Add rsp (*8 (length cm)))
                (Jmp next))
           cm))])]))

;; [Listof Pat] CEnv Symbol Nat -> (list Asm Asm CEnv)
(define (compile-struct-patterns ps cm next i)
  (match ps
    ['() (list '() '() cm)]
    [(cons p ps)
     (match (compile-pattern p cm next)
       [(list i1 f1 cm1)
        (match (compile-struct-patterns ps cm1 next (add1 i))
          [(list is fs cmn)
           (list
            (seq (Mov rax (Offset rax (*8 i)))
                 i1
                 (Mov rax (Offset rsp (*8 (sub1 (length cm1)))))
                 is)
            (seq f1 fs)
            cmn)])])]))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; compile-define.rkt

;; (provide (all-defined-out))
;; (require "ast.rkt"
;;         "types.rkt"
;;         "fv.rkt"
;;         "utils.rkt"
;;         "compile-expr.rkt"
;;         a86/ast)

;; (define r9 'r9)
;; (define r15 'r15)

;; [Listof Defn] -> [Listof Id]
(define (define-ids ds)
  (match ds
    ['() '()]
    [(cons (Defn f l) ds)
     (cons f (define-ids ds))]))

;; [Listof Defn] GEnv -> Asm
(define (compile-defines ds g)
  (match ds
    ['() (seq)]
    [(cons d ds)
     (seq (compile-define d g)
          (compile-defines ds g))]))

;; Defn GEnv -> Asm
(define (compile-define d g)
  (match d
    [(Defn f e)
     (seq ; (%%% (symbol->string f))
          (Data)
          (Label (symbol->label f))
          (Dq 0)
          (Text)
          (compile-e e '() g #f)
          (Mov (Offset (symbol->label f) 0) rax))]))

;; [Listof Lam] GEnv -> Asm
(define (compile-lambda-defines ls g)
  (match ls
    ['() (seq)]
    [(cons l ls)
     (seq (compile-lambda-define l g)
          (compile-lambda-defines ls g))]))

;; Lambda GEnv -> Asm
(define (compile-lambda-define l g)
  (let ((fvs (fv- l g)))    
    (match l
      [(Lam f xs e)
       (let ((env (append (reverse fvs) (reverse xs) (list #f))))
         (seq (Label (symbol->label f))
              (Cmp r15 (length xs))
              (Jne 'raise_error_align)              
              (Mov rax (Offset rsp (*8 (length xs))))
              (Xor rax type-proc)
              (copy-env-to-stack fvs 8)              
              (compile-e e env g #t)
              (Add rsp (*8 (length env))) ; pop env
              (Ret)))]
      [(LamRest f xs x e)
       (let ((env (append (reverse fvs) (cons x (reverse xs)) (list #f))))
         (seq (Label (symbol->label f))
              (Cmp r15 (length xs))
              (Jl 'raise_error_align)
              
              (Sub r15 (length xs))
              (Mov rax val-empty)
              (let ((loop (gensym))
                    (done (gensym)))
                (seq (Label loop)
                     (Cmp r15 0)
                     (Je done)
                     (Mov (Offset rbx 0) rax)
                     (Pop rax)
                     (Mov (Offset rbx 8) rax)
                     (Mov rax rbx)
                     (Or rax type-cons)
                     (Add rbx 16)
                     (Sub r15 1)
                     (Jmp loop)
                     (Label done)))
              (Push rax)
              
              (Mov rax (Offset rsp (*8 (add1 (length xs)))))
              (Xor rax type-proc)
              (copy-env-to-stack fvs 8)              
              (compile-e e env g #t)
              (Add rsp (*8 (length env))) ; pop env
              (Ret)))]
    [(LamCase f cs)
     (seq ; (%%% "lamcase code")
          (Label (symbol->label f))
          (compile-fun-case-select cs)
          (Jmp 'raise_error_align)
          (compile-fun-case-clauses cs g))])))

(define (compile-fun-case-clauses cs g)
  (append-map (lambda (c) (compile-lambda-define c g)) cs))

(define (compile-fun-case-select cs)
  (append-map compile-fun-case-selector cs))

(define (compile-fun-case-selector c)
  (match c
    [(Lam f xs e)
     (seq (Cmp r15 (length xs))
          (Je (symbol->label f)))]
    [(LamRest f xs x e)
     (seq (Mov r9 (sub1 (length xs)))
          (Cmp r9 r15)
          (Jl (symbol->label f)))]))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; compile-literals.rkt

(provide compile-literals init-symbol-table literals)
(require "ast.rkt"
         "utils.rkt"
         a86/ast)

;; (define rdi 'rdi)

;; Prog -> Asm
(define (compile-literals p)
  (append-map compile-literal (literals p)))

;; Symbol -> Asm
(define (compile-literal s)
  (let ((str (symbol->string s)))
    (seq (Label (symbol->data-label s))
         (Dq (string-length str))
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
       (Call 'intern_symbol)))

;; Prog -> [Listof Symbol]
(define (literals p)
  (remove-duplicates
   (map to-symbol (literals* p))))

;; Prog -> [Listof Symbol]
(define (symbols p)
  (remove-duplicates (filter symbol? (literals* p))))

;; (U String Symbol) -> Symbol
(define (to-symbol s)
  (if (string? s)
      (string->symbol s)
      s))

;; Prog -> [Listof (U Symbol String)]
(define (literals* p)
  (match p
    [(Prog ds)
     (append-map literals-d ds)]))

;; Defn -> [Listof (U Symbol String)]
(define (literals-d d)
  (match d
    [(Defn f l)
     (literals-e l)]))

;; Expr -> [Listof (U Symbol String)]
(define (literals-e e)
  (match e
    [(Quote d) (literals-datum d)]
    [(Prim p es)
     (append-map literals-e es)]
    [(If e1 e2 e3)
     (append (literals-e e1) (literals-e e2) (literals-e e3))]
    [(Begin e1 e2)
     (append (literals-e e1) (literals-e e2))]
    [(Let xs es e)
     (append (append-map literals-e es) (literals-e e))]
    [(App e1 es)
     (append (literals-e e1) (append-map literals-e es))]
    [(Lam f xs e)
     (literals-e e)]
    [(LamRest f xs x e1)
     (literals-e e1)]
    [(LamCase f cs)
     (append-map literals-e cs)]
    [(Match e ps es)
     (append (literals-e e) (append-map literals-match-clause ps es))]
    [_ '()]))

;; Pat Expr -> [Listof Symbol]
(define (literals-match-clause p e)
  (append (literals-pat p) (literals-e e)))

;; Pat -> [Listof (U Symbol String)]
(define (literals-pat p)
  (match p
    [(PSymb s) (list s)]
    [(PStr s) (list s)]
    [(PBox p) (literals-pat p)]
    [(PCons p1 p2) (append (literals-pat p1) (literals-pat p2))]
    [(PAnd p1 p2) (append (literals-pat p1) (literals-pat p2))]
    [(PPred e) (literals-e e)]
    [_ '()]))

;; Datum -> [Listof (U Symbol String)]
(define (literals-datum d)
  (cond
    [(string? d) (list d)]
    [(symbol? d) (list d)]
    [(cons? d)
     (append (literals-datum (car d))
             (literals-datum (cdr d)))]
    [(box? d)
     (literals-datum (unbox d))]
    [(vector? d)
     (append-map literals-datum (vector->list d))]
    [else '()]))

;; [Listof Char] -> Asm
(define (compile-string-chars cs)
  (match cs
    ['() (seq)]
    [(cons c cs)
     (seq (Dd (char->integer c))
          (compile-string-chars cs))]))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; compile.rkt

(provide (all-defined-out))
(require "ast.rkt"
         "types.rkt"
         "lambdas.rkt"
         "fv.rkt"
         "utils.rkt"
         "compile-define.rkt"
         "compile-expr.rkt"
         "compile-literals.rkt"
         a86/ast)

;; Registers used
;; (define rbx 'rbx) ; heap
;; (define rsp 'rsp) ; stack
;; (define rdi 'rdi) ; arg

;; type CEnv = [Listof Id]

(define (compile p)
  (match p
    [(Prog ds)
     (let ((gs (append stdlib-ids (define-ids ds))))
       (seq (externs)
            (map (lambda (i) (Extern (symbol->label i))) stdlib-ids)
            (Global 'entry)
            (Label 'entry)
            (Mov rbx rdi) ; recv heap pointer
            (init-symbol-table p)
            (init-lib)
            
            (compile-defines ds gs)
            (compile-variable (last-define-id ds) '() gs)
            (Ret)
            (compile-lambda-defines (lambdas p) gs)
            (Global 'raise_error_align)
            (Label 'raise_error_align)
            pad-stack
            (Mov rdi 0) ; null arg
            (Call 'raise_error)

          ;; one way to make `cons' a function instead of a primitive
          ;;cons-function
          
          (Data)
          (compile-literals p)))]))

(define (last-define-id ds)
  (match ds
    [(cons (Defn x _) '()) x]
    [(cons d ds) (last-define-id ds)]))

(define (init-lib)
  (let ((r (gensym))) ; call init_lib
    (seq (Extern 'init_lib)
         (Lea rax r)
         (Push rax)
         (Jmp 'init_lib)
         (Label r))))

(define stdlib-ids
  '(list list* make-list list? foldr map filter length append
         memq member append-map vector->list
         reverse
         number->string gensym read read-char
         > <= >=
         void?
         list->string string->list
         char<=?
         remove-duplicates remq* remove* remove
         andmap vector list->vector boolean?
         substring odd?
         ;; Op0
         read-byte peek-byte void
         ;; Op1
         add1 sub1 zero? char? write-byte eof-object?
         integer->char char->integer
         box unbox box? empty? cons? car cdr
         vector? vector-length string? string-length
         symbol->string string->symbol symbol?
         string->uninterned-symbol
         open-input-file
         write-char error integer?
         eq-hash-code
         ;; Op2
         + - < = cons eq? make-vector vector-ref
         make-string string-ref string-append
         quotient remainder set-box!
         bitwise-and bitwise-ior bitwise-xor arithmetic-shift         
         ;; Op3
         vector-set!))

(define (externs)
  (map Extern
       '(peek_byte
         read_byte
         write_byte
         raise_error
         intern_symbol
         symb_cmp
         string_append
         memcpy
         open_input_file
         read_byte_port
         peek_byte_port)))

(define cons-function
  (let ((code (gensym 'cons_code))
        (clos (gensym 'cons_closure)))
    (seq (Data)
         (Label (symbol->label 'cons))
         (Dq (Plus (symbol->label clos) type-proc))
         (Label (symbol->label clos))
         (Dq (symbol->label code))
         (Text)
         (Label (symbol->label code))
         (Pop rax)
         (Mov (Offset rbx 0) rax)
         (Pop rax)
         (Mov (Offset rbx 8) rax)
         (Add rsp 8) ; pop function
         (Mov rax rbx)
         (Or rax type-cons)
         (Add rbx 16)
         (Ret))))


;; Lib -> Asm
(define (compile-library l)
  (match l
    [(Lib ids ds)
     (let ((g (define-ids ds)))
       (seq (externs)
            (map (lambda (i) (Global (symbol->label i))) ids)
            (Extern 'raise_error_align)
            
            (Global 'init_lib)
            (Label 'init_lib)
            (compile-defines ds g)
            (Ret)
            
            (compile-lambda-defines (lambdas-ds ds) g)
            (Data)
            (compile-literals (Prog ds))))]))

;(parse-e '(add1 x))