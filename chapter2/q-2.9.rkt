#lang racket

(require "q-2.7.rkt")

;; (Q) The width of an interval is half of the difference between its
;; upper and lower bounds. The with is a measure of the uncertainty of
;; the number specified by the interval. For some arithmetic
;; operations the width of the results of combining two intervals is a
;; function only of the widths of the argument intervals, whereas for
;; others the width of the combination is not a function of the widths
;; of the argument intervals. Show that the width of the sum (or
;; difference) of two intervals is a function only of the widths of
;; the intervals being added (or subtracted). Give examples to show
;; that this is not true for multiplication or division.

;; (A) First, just for completeness, here is a definition of a width
;; procedure:

(define (width x)
  (/ (- (upper-bound x) (lower-bound x)) 2))

;; We could use substitution to show the claim for add-interval and
;; sub-interval, but that's a bit notationally long-winded, so lets
;; use mathematical notation instead, where we represent an interval
;; as an ordered pair (a,b) where a <= b. Alyssa's definition of
;; add-interval is equivalent to the definition

;; (l1, u1) + (l2, u2) = (l1 + l2, u1 + u2)

;; and our definition of width above is equivalent to the definition

;; width (l,u) = (u - l)/2

;; Therefore,

;; width ((l1,u1) + (l2,u2))
;; = width (l1 + l2, u1 + u2)       by the definition of addition
;; = ((u1 + u2) - (l1 + l2)) / 2    by the definition of width
;; = (u1 - l1)/2 + (u2 - l2)/2      by arithetic laws
;; = width (l1,u1) + width (l2,u2)  by the definition of width

;; Thus, the width of a sum is determined solely by the widths of the
;; two operands.

;; Similarly, the definition of sub-interval from question 2.8 is
;; equivalent to the definition

;; (l1, u1) - (l2, u2) = (l1 - u2, u1 - l2)

;; and therefore

;; width ((l1, u1) - (l2,u2))
;; = width (l1 - u2, u1 - l2)          by the definition of subtraction
;; = ((u1 - l2) - (l1 - u2)) / 2       by the definition of width
;; = (u1 - l1)/2 + (u2 - l2)/2         by arithmetic laws
;; = width (l1, u1) + width (l2, u2)   by the definition of width

;; As with sums, the width of a difference is determined solely by the
;; widths of the two operands.

;; To show that this is not true for multiplication or division,
;; consider multiplying the intervals (1,2) and (10,11), and then the
;; intervals (5,6) and (10,11). All these intervals have width 1; if
;; the width of a product depends only on the widths of the operands,
;; then these two multiplications should have the same width. From
;; Alyssa's definition of multiplication, we have

;; (l1, u1) * (l2, u2) = (min[l1*l2, l1*u2, u1*l2, u1*u2],
;;                        max[l1*l2, l1*u2, u1*l2, u1*u2])

;; We have

;; (1,2) * (10,11)
;;   = (min[10, 11, 20, 22], max[10, 11, 20, 22])
;;   = (10, 22)

;; with a width of (22-10)/2 = 6

;; Meanwhile,

;; (5,6) * (10,11)
;;   = (min[50, 55, 60, 66], max[50, 55, 60, 66])
;;   = (50, 66)

;; with a width of (66-50)/2 = 8.

;; Thus, despite all the operand intervals being width 1, we have
;; different widths for the two different products.

;; We see something similar for division:

;; (1,2) / (10,11)
;;   = (1,2) * (1/11, 1/10)
;;   = (1/11, 1/5)

;; with a width of

;; (1/5 - 1/11)/2
;;   = (11 - 5)/110
;;   = 6/110
;;   = 3/55

;; whereas for the other quotient, we have

;; (5,6) / (10,11)
;;   = (5,6) * (1/11, 1/10)
;;   = (5/11, 3/5)

;; with a width of

;; (3/5 - 5/11)/2
;;   = (33 - 25)/110
;;   = 8/110
;;   = 4/55

;; The first arguments for the two quotients are width 1, and the
;; second arguments for the two quotients are identical. Since the
;; widths of the two divisions are different, the width of a division
;; therefore cannot depend only on the widths of its operands.
