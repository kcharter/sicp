#lang racket

(require "fixed.rkt")
(require "q-1.43.rkt")

(provide log-2 pow nth-root)

;; Figuring out how many times we need to average damp to find n-th
;; roots by fixed-point iteration.

;; First, let's figure out what the problem is for the single average
;; damped version. We want to do a fixed-point iteration on the
;; function

;;  f(y) = (y + x/y^(n-1))/2
;;       = (y + x*y^(1-n))/2

;; Now, the derivative of this function is

;;  f'(y) = (1 + (1-n)*x*y^(-n))/2

;; Consider the value of f'(x^(1/n)), i.e. at the nth root of x, the
;; fixed point of f:

;;  f'(x^(1/n)) = (2 - n)/2

;; Note that x disappears and the value of f' at the fixed point
;; depends only on n. We have

;;  n = 2 =>  f' = 0
;;  n = 3 =>  f' = -1/2
;;  n = 4 =>  f' = -1
;;  n = 5 =>  f' = -3/2

;; In fact, as n increases, f'(x^(1/n)) becomes more negative.

;; Now, the problem is that from n = 4 onward, there is a closed
;; interval containing the fixed point in which f' is less than or
;; equal to -1; for n > 4, there is an open interval containing the
;; fixed point in which f' < -1. In such an open region, the fixed
;; point iteration will not converge -- evaluating f(x_i) gets us
;; x_(i+1) which is further away from the fixed point. My foggy memory
;; of dynamical systems is that there is a theorem about the
;; convergence of fixed-point iterations where |f'| < 1 in a
;; neighbourhood around the fixed point is a necessary condition for
;; convergence.

;; So, the solution to our problem is to average-damp enough times
;; that f' > -1 at the fixed point.

;; Let

;;  f_0(y) = x/y^(n-1)

;; and let

;;  f_i(y) = (y + f_[i-1](y))/2

;; be the ith average damping of f. The derivative is

;;  f_0'(y) = (1-n)*x/y^n

;;  f_i'(y) = 1/2 + (1/2) * f_[i-1]'(y)

;; and we can show by induction that

;;  f_i'(y) = Sum[j=1,i](1/2^j) + (1/2^i) * (1-n) * x/y^n

;;          = 1 - 1/2^i + (1/2^i) * (1-n) * x/y^n

;; Consequently, the derivative at the fixed point is

;;  f_i'(x^(1/n)) = 1 - n/2^i

;; For f_i'(x^(1/n)) > -1, we can work out that

;;  i > log_2(n) - 1

;; and so the minimum required number of average dampings is

;;  i_min = ceiling(log_2(n) - 1)

;; Note that

;;  x = 2 ^ log_2(x) = (e ^ ln(2)) ^ log_2(x)
;;                   = e ^ (ln(2) * log_2(x))

;; and so

;;  ln(x) = ln(2) * log_2(x)

;; or

;;  log_2(x) = ln(x) / ln(2)

;; This permits us to compute base-2 logarithms given a function for
;; computing natural logarithms.

(define (log-2 x)
  (/ (log x) (log 2)))

(define (pow k x)
  ((repeated (lambda (z) (* x z)) (- k 1)) x))

(define (nth-root n x)
  (define i (ceiling (log-2 (- n 1))))
  (define (f y) (/ x (pow (- n 1) y)))
  (fixed-point ((repeated average-damp i) f) 1.0))