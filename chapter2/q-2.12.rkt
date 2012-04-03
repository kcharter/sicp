#lang racket

(require "q-2.7.rkt")
(provide make-center-percent center width percent)

;; (Q) Define a constructor make-center-percent that takes a center
;; and a percentage tolerance and produces the desired interval. You
;; must also define a selector percent that produces the percentage
;; tolerance for a given interval. The center selector is the same as
;; the one shown above (in the text).

;; First, here are the definitions of center and width from the text:

(define (center i)
  (/ (+ (lower-bound i) (upper-bound i)) 2))

(define (width i)
  (/ (- (upper-bound i) (lower-bound i)) 2))

;; and here are the definitions of make-center-percent and percent:

(define (make-center-percent c p)
  (let ((w (abs (* c (/ p 100.0)))))
    (make-interval (- c w) (+ c w))))

(define (percent i)
  (* 100.0 (/ (width i) (center i))))
