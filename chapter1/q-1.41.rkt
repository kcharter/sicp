#lang racket

(provide double)

(define (double f)
  (lambda (x) (f (f x))))

(define (inc x) (+ 1 x))

;; the expression below evaluates to 21. We can also figure this out
;; through hand evaluation

; (((double (double double)) inc) 5)

