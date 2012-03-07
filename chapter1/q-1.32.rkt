#lang racket

;; define 'accumulate', a generalization of 'sum' and 'product'

(define (accumulate combiner null-value term a next b)
  (if (> a b)
      null-value
      (combiner (term a) (accumulate combiner null-value
                                     term (next a) next b))))

(define (sum term a next b)
  (accumulate + 0 term a next b))

(define (product term a next b)
  (accumulate * 1 term a next b))

;; iterative accumulate:

(define (accumulate-i combiner null-value term a next b)
  (define (go x r)
    (if (> x b)
        r
        (go (next x) (combiner r (term x)))))
  (go a null-value))

(define (sum-i term a next b)
  (accumulate-i + 0 term a next b))

(define (product-i term a next b)
  (accumulate-i * 1 term a next b))

(define (identity x) x)
(define (inc x) (+ 1 x))

(display "sum from 1 to 10: ")
(display (sum identity 1 inc 10))
(newline)
(display "sum-i from 1 to 10: ")
(display (sum-i identity 1 inc 10))
(newline)
(display "factorial 5: ")
(display (product identity 1 inc 5 ))
(newline)
(display "iterative factorial 5: ")
(display (product-i identity 1 inc 5))
(newline)
