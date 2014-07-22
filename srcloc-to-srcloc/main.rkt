#lang racket/base

(provide here
         (all-from-out "srcloc-to-srcloc.rkt"))

(require "srcloc-to-srcloc.rkt"
         (for-syntax racket/base
                     "srcloc-to-srcloc.rkt"))

(module+ test
  (require rackunit racket/match))

(define-syntax here
  (lambda (stx)
    (define loc (srcloc->srcloc stx))
    (datum->syntax stx loc (srcloc->list loc) stx)))



(module+ test
  (check-match here (srcloc (== (build-path (current-directory) "main.rkt")) 21 15 _ 4)))