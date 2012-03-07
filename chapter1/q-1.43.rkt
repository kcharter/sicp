#lang racket

(require "powers.rkt")
(require "q-1.42.rkt")

(provide repeated repeated-i)


(define (show-rep repf repf-name f f-name n x)
  (display repf-name)
  (display " of ")
  (display f-name)
  (display " applied to ")
  (display x)
  (display " = ")
  (display ((repf f n) x))
  (newline))

;; write a 'repeated' function for the iterated application of another
;; function f some number of times

(define (repeated f n)
  (cond ((= 0 n) (lambda (x) x))
        ((= 1 n) f)
        (#true (compose f (repeated f (- n 1))))))

(define (show-repeated f f-name n x)
  (show-rep repeated "repeated" f f-name n x))


;; there is more than one way to define this; for example, we need not
;; use 'compose' at all

(define (repeated-i f n)
  (lambda (x)
    (define (go n r)
      (if (<= n 0)
          r
          (go (- n 1) (f r))))
    (go n x)))

(define (show-repeated-i f f-name n x)
  (show-rep repeated' "repeated'" f f-name n x))


(show-repeated square "square" 3 2.0)
(show-repeated-i square "square" 3 2.0)



