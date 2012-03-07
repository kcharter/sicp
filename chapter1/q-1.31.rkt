#lang racket

(require "sum.rkt")

;; part a: recursive product
(define (product term a next b)
  (if (> a b)
      1
      (* (term a) (product term (next a) next b))))

(define (factorial n)
  (product identity 1 inc n))

(display "factorial of 5: ")
(display (factorial 5))
(newline)

(define (pi-approx n)
  (define (square x) (* x x))
  (define (frac n)
    (define two-n (* 2.0 n))
    (/ (* two-n (+ 2.0 two-n))
       (square (+ 1.0 two-n))))
  (* 4.0
     (product frac 1 inc n)))

(define (show-pi-approx n)
  (display "pi-approx ")
  (display n)
  (display ": ")
  (display (pi-approx n))
  (newline))

(show-pi-approx 5)
(show-pi-approx 10)
(show-pi-approx 15)
(show-pi-approx 20)
(show-pi-approx 25)
(show-pi-approx 30)
(show-pi-approx 40)
(show-pi-approx 50)
(show-pi-approx 100)

;; part b: iterative product

(define (product-i term a next b)
  (define (go x r)
    (if (> x b)
        r
        (go (next x) (* r (term x)))))
  (go a 1))

(display "iterative factorial of 5: ")
(display (product-i identity 1 inc 5))
(newline)



