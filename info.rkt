#lang info
(define version "1.0")
(define collection 'multi)
(define deps (list "base" "rackunit" "redex-lib"
  "https://github.com/cmsc430/a86.git?path=#main"))
(define build-deps
  (list "https://github.com/cmsc430/a86.git?path=#main"))


;; Outlaw is omitted here because it depends on libraries that are a pain
;; to ensure are set up properly and we don't want students to see failing
;; tests at the beginning of the semester, nor do we want to get into
;; setting up libraries only needed in the last week and only if you
;; actually care to run Outlaw.

;; To test these, you should do an explicit: raco test -c <lang>
(define test-omit-paths (list "iniquity-gc"
                              "mountebank"
                              ;; "mug"  ;; NOTE: seems to pass currently
                              "neerdowell"
                              "outlaw"))
