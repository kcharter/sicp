#lang racket

(require "q-2.7.rkt")

(provide sub-interval)

;; (Q) Using reasoning analogous to Alyssa's, describe how the
;; difference of two intervals may be computed. Define a corresponding
;; subtraction procedure, called sub-interval.

;; (A) Suppose that the bounds are (l1, u1) and (l2, u2). The
;; lower-bound of the result should be the least of the four differences

;; l1 - l2
;; l1 - u2
;; u1 - l2
;; u1 - u2

;; while the upper bound should be the greatest of these
;; differences. Now, we know that l1 <= u1 and l2 <= u2, and we know
;; also that

;; a <= b implies x - b <= x - a
;; a <= b implies a - x <= b - x

;; Therefore,

;; l1 - u2 <= l1 - l2 <= u1 - l2
;; l1 - u2 <= u1 - u2 <= u1 - l2

;; so l1 - u2 is the overall minimum, and u1 - l2 is the overall
;; maximum, and we have

(define (sub-interval x y)
  (make-interval (- (lower-bound x) (upper-bound y))
                 (- (upper-bound x) (lower-bound y))))

