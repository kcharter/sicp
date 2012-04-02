#lang racket

(provide make-interval upper-bound lower-bound)

;; Here is Alyssa's definition of make-interval:

(define (make-interval a b) (cons a b))

;; (Q) Define upper-bound and lower-bound.

;; (A) The make-interval constructor does not ensure that (<= a b),
;; for example, so we'll make the selectors a little bit fancy and
;; ensure that (<= (lower-bound (make-interval a b)) (upper-bound
;; (make-interval a b))) for all 'a' and 'b'.

(define (lower-bound i)
  (min (car i) (cdr i)))

(define (upper-bound i)
  (max (car i) (cdr i)))

