#lang racket

(require "q-2.7.rkt")

(provide mul-interval)

;; (Q) In passing, Ben also cryptically comments: "By testing the
;; signs of the endpoints of the intervals, it is possible to break
;; mul-interval into nine cases, only one of which requires more than
;; two multiplications." Rewrite this procedure using Ben's
;; suggestion.

;; (A) First, let's figure out what Ben is driving at. In a pair (a,b)
;; with a <= b, there are three possible combinations of signs:

;;  a < 0 and b < 0    (both negative)
;;  a < 0 and b >= 0   (one negative)
;;  a >= 0 and b >= 0  (no negatives)

;; Since in an arbitrary product each interval can be one of these
;; three combinations, there are a total of 3*3 = 9 combinations for
;; both operands.

;; Now, the breakdown is important, because for many of the various
;; cases we can figure out what the minimum and maximum of the four
;; different products of bounds should be, without computing all four
;; products and calling min and max. Here is a revised definition of
;; mul-interval, with comments for the nine individual cases.

(define (mul-interval x y)
  (let ((lx (lower-bound x))
        (ux (upper-bound x))
        (ly (lower-bound y))
        (uy (upper-bound y)))
    (cond ((< ux 0)
           ;; lx and ux are both negative, |lx| >= |ux|
           (cond ((< uy 0)
                  ;; all bounds are negative, so all products are
                  ;; positive and the greatest is the product of the
                  ;; lower bounds, while the least is the product
                  ;; of the upper bounds
                  (make-interval (* ux uy) (* lx ly)))
                 ((< ly 0)
                  ;; lx*ly and ux*ly are non-negative, lx*ly is greatest
                  ;; lx*uy and ux*uy are negative, lx*uy is least
                  (make-interval (* lx uy) (* lx ly)))
                 (else
                  ;; all products are negative, so the least is lx*uy
                  ;; and the greatest is ux*ly
                  (make-interval (* lx uy) (* ux ly)))))
          ((< lx 0)
           ;; just lx is negative
           (cond ((< uy 0)
                  ;; both ly and uy are negative, so lx*ly >= lx*uy
                  ;; are non-negative, and ux*ly <= ux*uy are both
                  ;; negative
                  (make-interval (* ux ly) (* lx ly)))
                 ((< ly 0)
                  ;; just ly is negative; thus lx*ly and ux*uy are
                  ;; both non-negative, but it's not clear which is
                  ;; largest. lx*uy and ux*ly are both non-positive,
                  ;; but it's not clear which is smallest.
                  (make-interval (min (* lx uy) (* ux ly))
                                 (max (* lx ly) (* ux uy))))
                 (else
                  ;; ly and uy are non-negative. Thus lx*uy <= lx*ly
                  ;; are both negative, while ux*ly <= ux*uy are both
                  ;; non-negative.
                  (make-interval (* lx uy) (* ux uy)))))
          (else
           ;; both lx and ux are non-negative
           (cond ((< uy 0)
                  ;; both ly and uy are negative, so all four products
                  ;; are non-positive, the least is ux*ly and the greatest
                  ;; is lx*uy
                  (make-interval (* ux ly) (* lx uy)))
                 ((< ly 0)
                  ;; just ly is negative. Thus ux*ly <= lx*ly are
                  ;; non-positive, and lx*uy <= ux*uy are
                  ;; non-negative.
                  (make-interval (* ux ly) (* ux uy)))
                 (else
                  ;; all bounds are non-negative: the least is lx*ly,
                  ;; the greatest is ux*uy
                  (make-interval (* lx ly) (* ux uy))))))))

;; As Ben says, only one case, the one where there is a single
;; negative bound in each operand, requires performing more than one
;; operation.
