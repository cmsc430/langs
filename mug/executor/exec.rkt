#lang racket
(require a86/interp)

(provide exec
         (struct-out exec-state)
         exec-unload
         call-with-exec)

(require a86/interp
         ffi/unsafe)
(require "decode.rkt")
(require "../runtime/types.rkt")
(struct exec-state (program heap) #:transparent)

(define (symb-ptr->string p)
  (define len (bits->value (ptr-ref p _uint64 0)))
  (define cp-base (ptr-add p 8 _byte))
  (build-string
   len
   (λ (i)
     (integer->char (ptr-ref cp-base _uint32 i)))))

(define (exec/state prog)
  (define intern-table (make-hash))
  (define (intern-symbol/cb p)
    (define s (symb-ptr->string p))
    (hash-ref! intern-table s (λ () p)))
  (define heap (malloc _int64 10000))
  (exec-state
   (parameterize       
       ([current-externs
         (list
          (extern 'read_byte read-byte (_fun -> _val))
          (extern 'peek_byte peek-byte (_fun -> _val))
          (extern 'write_byte write-byte (_fun _val -> _val))
          (extern 'raise_error
                  (λ () (raise 'err)) 
                  (_fun -> _void))
          (extern 'intern_symbol
                  intern-symbol/cb
                  (_fun _pointer -> _pointer)))])
     (asm-load prog))
   heap))

(define (exec-call st)
  (match-define (exec-state program heap) st)
  (with-handlers ([(λ (x) (eq? x 'err)) identity])
    (asm-call program 'entry heap)))

(define (exec-unload st)
  (asm-unload (exec-state-program st)))

;; ------------------------------------------------------------
;; public API

;; execute with runtime system and Racket host
;; return raw bits plus the live state needed to interpret them safely

;; CAUTION: this does not unload
(define (exec asm)
  (exec-call (exec/state asm)))

;; version of above that ensures unloading
(define (call-with-exec e f)
  (define st (exec/state e))
  (dynamic-wind
    void
    (λ () (f (exec-call st)))
    (λ () (exec-unload st))))

