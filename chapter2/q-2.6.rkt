#lang racket

;; Church numerals. Given procedures-as-values, we don't need a
;; special representation for numbers. Here are the definitions of
;; zero and the add-1 function for Church numerals:

(define zero (lambda (f) (lambda (x) x)))

(define (add-1 n)
  (lambda (f) (lambda (x) (f ((n f) x)))))

;; (Q) Define one and two directly (not in terms of zero and
;; add-1). Hint: Use substitution to evaluate (add-1 zero).

;; (A) OK, we'll take the hint.

;; (add-1 zero)
;; (lambda (f) (lambda (x) (f ((zero f) x))))
;; (lambda (f) (lambda (x) (f (((lambda (f) (lambda (x) x)) f) x))))
;; (lambda (f) (lambda (x) (f ((lambda (x) x) x))))
;; (lambda (f) (lambda (x) (f x)))

;; therefore, we have the definition

(define one (lambda (f) (lambda (x) (f x))))

;; I'm betting that two is (lambda (f) (lambda (x) (f (f x)))). But
;; let's establish the general case. Suppose that the nth Church
;; numeral is

;; n = (lambda (f) (lambda (x) (f^n x)))

;; where 'f^n' means the nth iterated application of 'f' (the identity
;; function for n = 0). This definition is consistent with the
;; definitions of zero and one above. What is the result of (add-1 n)?

;; (add-1 n)
;; (lambda (f) (lambda (x) (f ((n f) x))))
;; (lambda (f) (lambda (x) (f (((lambda (f) (lambda (x) (f^n x))) f) x))))
;; (lambda (f) (lambda (x) (f ((lambda (x) (f^n x)) x))))
;; (lambda (f) (lambda (x) (f (f^n x))))

;; but (f (f^n x)) = (f^[n+1] x), so the result is

;; (lambda (f) (lambda (x) (f^[n+1] x)))

;; So, this gives us a proof by induction that the nth Church numeral is

;; (lambda (f) (lambda (x) (f^n x)))

;; and so we have

(define two (lambda (f) (lambda (x) (f (f x)))))

;; (Q) Give a direct definition of the addition procedure '+' (not in
;; terms of repeated application of add-1).

;; add-1 works by applying f once to the result of ((n f) x). For a
;; Church numeral n, ((n f) x) eliminates the outer two lambdas,
;; exposing the iterated application of f. This suggests that the
;; definition of '+' can be:

(define (+ n m)
  (lambda (f) (lambda (x) ((n f) ((m f) x)))))

;; Let's confirm this by working through the evaluation with the nth
;; and mth Church numerals:

;; (+ n m)
;; (lambda (f) (lambda (x) ((n f) ((m f) x))))
;; (lambda (f) (lambda (x) ((n f) (((lambda (f) (lambda (x) (f^m x))) f) x))))
;; (lambda (f) (lambda (x) ((n f) ((lambda (x) (f^m x)) x))))
;; (lambda (f) (lambda (x) ((n f) (f^m x))))
;; (lambda (f) (lambda (x) (((lambda (f) (lambda (x) (f^n x))) f) (f^m x))))
;; (lambda (f) (lambda (x) ((lambda (x) (f^n x)) (f^m x))))
;; (lambda (f) (lambda (x) (f^n (f^m x))))
;; (lambda (f) (lambda (x) (f^[n+m] x)))

;; which is indeed the [n+m]th Church numeral.

