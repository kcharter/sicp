#lang racket

(require "sum.rkt")

;; (provide simpson)

(define (simpson f a b n)
  (let ((h (/ (- b a) n)))
    (let ((next (lambda (x) (+ x h h)))
          (two-f (lambda (x) (* 2 (f x))))
          (four-f (lambda (x) (* 4 (f x)))))
      (* (/ h 3.0)
         (+ (f a)
            (sum four-f (+ a h) next (- b h))
            (sum two-f (+ a h h) next (- b h h))
            (f b))))))

;; even with n=100, we get 0.25, which is more accurate than the
;; 'integral' function with dx=0.001

(display "integral of cube on [0,1], n=100: ")
(display (simpson cube 0 1 100))
(display "\n")
(display "integral of cube on [0,1], n=1000: ")
(display (simpson cube 0 1 1000))
(display "\n")

