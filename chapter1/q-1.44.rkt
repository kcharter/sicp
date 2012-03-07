#lang racket

(require "q-1.43.rkt")
(require "output.rkt")

(provide smooth n-fold-smooth show-smooths)

(define dx 0.0001)

;; a smoothing function

(define (smooth f)
  (lambda (x)
    (/ (+ (f (- x dx)) (f x) (f (+ x dx)))
       3.0)))

;; the n-fold smoothing function

(define (n-fold-smooth n f)
  ((repeated smooth n) f))


;; note that the amount of work done to compute a smoothed function
;; increasases exponentially with the number of smooths. Going much
;; beyond 10, 'show-smooths' will take a very long time

(define (show-smooths f x)
  (define (go n)
    (display-line (list "n="
                        n
                        " "
                        ((n-fold-smooth n f) x)))
    (if (>= n 10)
        (newline)
        (go (+ n 1))))
  (go 0))

        