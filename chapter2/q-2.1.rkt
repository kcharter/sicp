#lang racket

(provide make-rat numer denom print-rat)

;; define an improved version of make-rat that ensures that the
;; denominator is always positive
(define (make-rat n d)
  (let ((g (gcd n d))
        (negative (or (and (< 0 n) (> 0 d)) (and (> 0 n) (< 0 d)))))
    (let ((abs-numer (/ (abs n) g)))
      (cons (if negative (- abs-numer) abs-numer) (/ (abs d) g)))))

(define (numer x) (car x))

(define (denom x) (cdr x))

(define (print-rat x)
  (newline)
  (display (numer x))
  (display "/")
  (display (denom x)))

