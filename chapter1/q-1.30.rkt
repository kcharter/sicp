#lang racket

(require "sum.rkt")

;; iterative version of 'sum'

(define (sum-i term a next b)
  (define (go x r)
    (if (> x b)
        r
        (go (next x) (+ r (term x)))))
  (go a 0))

(define (sum-i-integers a b)
  (sum-i identity a inc b))

(display "iterative sum of integers from 1 to 1000: ")
(display (sum-i-integers 1 1000))
(newline)
(display "recursive sum of integers from 1 to 1000: ")
(display (sum-integers 1 1000))
(newline)

    