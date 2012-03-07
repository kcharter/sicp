#lang racket

(require "powers.rkt")

(provide iterative-improve sqrt fixed-point)

(define (iterative-improve good-enough? improve)
  (lambda (initial-guess)
    (define (go guess)
      (if (good-enough? guess)
          guess
          (go (improve guess))))
    (go initial-guess)))

;; rewritten sqrt from section 1.1.7

;; taken from section 1.1.7
(define (average x y)
  (/ (+ x y) 2))

(define (sqrt x)
  ;; the definitions of 'good-enough?' and 'improve' are taken from
  ;; section 1.1.7, except that there 'x' is a second parameter to
  ;; 'improve'
  (define (good-enough? guess)
    (< (abs (- (square guess) x)) 0.001))
  (define (improve guess)
    (average guess (/ x guess)))
  ((iterative-improve good-enough? improve) 1.0))

;; implement the 'fixed-point' procedure from section 1.3.3 in terms
;; of 'iterative-improve'

(define (fixed-point f first-guess)
  ;; here 'close-enough?' is taken from the original definition,
  ;; except that it cannot take the last two guesses as
  ;; inputs. Instead, we compute the difference between the guess and
  ;; the next iteration of 'f'. This means we'll evaluate 'f' more
  ;; times than the original version, which is a good argument for
  ;; using the original 'fixed-point' instead. The 'improve' function
  ;; is just 'f' itself.
  (define tolerance 0.00001)
  (define (close-enough? guess)
    (< (abs (- guess (f guess))) tolerance))
  ((iterative-improve close-enough? f) first-guess))
