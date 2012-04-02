#lang racket

(require "q-2.7.rkt")
(require "alyssa-defs.rkt")

(provide div-interval)

;; (Q) Ben Bitdiddle, and expert systems programmer, looks over
;; Alyssa's shoulder and comments that it is not clear what it means
;; to divide by an interval that spans zero. Modify Alyssa's code to
;; check for this condition and to signal an error if it occurs.

;; (A) We modify Alyssa's original by checking whether the second
;; operand contains zero between its bounds, inclusive.

(define (div-interval x y)
  (if (and (<= (lower-bound y) 0) (<= 0 (upper-bound y)))
      (error "div-interval: cannot divide by an interval containing zero:" y)
      (mul-interval x
                    (make-interval (/ 1.0 (upper-bound y))
                                   (/ 1.0 (lower-bound y))))))
