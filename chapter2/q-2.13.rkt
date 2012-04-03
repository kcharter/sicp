#lang racket

(require "q-2.7.rkt")
(require "q-2.12.rkt")

(provide tolerance)

;; (Q) Show that under the assumption of small percentage tolerances
;; there is a simple formula for the approximate percentage tolerance
;; of the product of two intervals in terms of the tolerances of the
;; factors. You may simplify the problem by assuming that all numbers
;; are positive.

;; (A) We'll make a mathematical argument and then confirm with a
;; little computation.

;; First, let's look at a fractional tolerance T, defined as the ratio
;; of the width to the center of an interval

(define (tolerance i)
  (/ (width i) (center i)))

;; mathematically,

;;  T(l,u) = (u-l)/(u+l)

;; where the factors of 1/2 in the width and the center cancel out.

;; Now, assume we have intervals (l1,u1) and (l2,u2), where all the
;; bounds are positive. Then the product is

;; (l1,u1) * (l2,u2) = (l1*l2, u1*u2)

;; and the tolerance T is

;; T(l1*l2, u1*u2)
;;   = (u1*u2 - l1*l2)/(u1*u2 + l1*l2)

;; Now, notice that

;; T(l,u) = (u-l)/(u+l)
;;        = (u/l - 1) / (u/l + 1)

;; and so we can solve for u/l in terms of T(l,u):

;; T(l,u) * (u/l + 1) = u/l - 1
;; implies  (u/l) * (1 - T(l,u)) = T(l,u) + 1
;; implies

;;     u/l = (1 + T(l,u)) / (1 - T(l,u))

;; Returning to the tolerance of the product, we have

;; T(l1*l2, u1*u2)
;;   = (u1*u2 - l1*l2)/(u1*u2 + l1*l2)
;;   = ((u1/l1) * (u2/l2) - 1) / ((u1/l1) * (u2/l2) + 1)
;;   = (((1 + T(l1,u1) / (1 - T(l1,u1))) * (1 + T(l2,u2)) / (1 - T(l2,u2))) - 1) /
;;     (((1 + T(l1,u1) / (1 - T(l1,u1))) * (1 + T(l2,u2)) / (1 - T(l2,u2))) + 1)

;; we can factor out 1 / ((1-T(l1,u1)) * (1-T(l2,u2))) in both the
;; numerator. The numerator becomes

;; ((1 + T(l1,u1)) * (1 + T(l2,u2))) - ((1 - T(l1,u1)) * (1 - T(l2,u2)))
;;  = 1 + T(l2,u2) + T(l1,u1) + T(l1,u1)*T(l2,u2) - 1 + T(l2,u2) + T(l1,u1) - T(l1,u1)*T(l2,u2)
;;  = 2 * (T(l1,u1) + T(l2,u2))

;; Similarly, the numerator becomes

;; ((1 + T(l1,u1)) * (1 + T(l2,u2))) + ((1 - T(l1,u1)) * (1 - T(l2,u2)))
;;  = 1 + T(l2,u2) + T(l1,u1) + T(l1,u1)*T(l2,u2) + 1 - T(l2,u2) - T(l1,u1) + T(l1,u1)*T(l2,u2)
;;  = 2 * (1 + T(l1,u1)*T(l2,u2))

;; and so the factors of two cancel and we have

;; T((l1,u1) * (l2,u2)) = (T(l1,u1) + T(l2,u2)) / (1 + T(l1,u1)*T(l2,u2))

;; When the tolerances are small, the factor T(l1,u1)*T(l2,u2) doesn't
;; contribute much to the denominator, and so we have the approximation

;; T((l1,u1) * (l2,u2)) ~ T(l1,u1) + T(l2,u2)

;; or, put slightly less verbosely,

;; T(R1 * R2) ~ T(R1) + T(R2)

;; Thus, the tolerance of a product is, for small tolerances, roughly
;; the sum of the operand tolerances.

;; Before leaving the question of products, there is one more fact to note about the tolerance T.

;; T(1/(l,u)) = T(1/u, 1/l) = (1/l - 1/u) / (1/l + 1/u) = (u - l) / (u + l) = T(l,u)

;; That is, the intervals R and 1/R have the same tolerance (and
;; percentage tolerance). Since we define division R1/R2 as R1 *
;; (1/R2), this mean that for small tolerances we also have
;; approximately

;; T(R1/R2) ~ T(R1) + T(R2)
