#lang racket

(provide cons car cdr)

;; An alternative procedural representation of pairs.

;; Here is an alternative definition of 'cons' and 'car':

(define (cons x y)
  (lambda (m) (m x y)))

(define (car z)
  (z (lambda (p q) p)))

;; (Q) Verify that (car (cons x y)) yields x for any objects x and y.

;; (A) We'll do this by simulating steps in the evaluation of (car (cons x y)):

;; (car (cons x y))
;; (car (lambda (m) (m x y)))
;; ((lambda (m) (m x y)) (lambda (p q) p))
;; ((lambda (p q) p) x y)
;; x

;; Therefore, for any x and y, (car (cons x y)) is x

;; (Q) What is the corresponding definition of cdr?

;; (A) Here is the definition:

(define (cdr z)
  (z (lambda (p q) q)))

;; That is, we apply z to a function of two arguments that returns its
;; second argument.

;; We want to prove that (cdr (cons x y)) is y for any objects x and
;; y. We do this again by working through the steps in the evaluation
;; of (cdr (cons x y)):

;; (cdr (cons x y))
;; (cdr (lambda (m) (m x y)))
;; ((lambda (m) (m x y)) (lambda (p q) q))
;; ((lambda (p q) q) x y)
;; y

;; and so, for any objects x and y (cdr (cons x y)) is y.


