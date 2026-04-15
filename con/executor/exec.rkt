#lang racket
(require a86/interp)

(provide exec
         (struct-out exec-state)
         exec-unload
         call-with-exec)

(require a86/interp
         ffi/unsafe)

(struct exec-state (program) #:transparent)

(define _val _int64)

(define (exec/state prog)
  (exec-state
   (asm-load prog)))

(define (exec-call st)
  (match-define (exec-state program) st)
  (asm-call program 'entry))

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

