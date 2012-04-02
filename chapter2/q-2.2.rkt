#lang racket

;; a representation of points line segments in a plane

(provide make-point x-point y-point print-point
         make-segment start-segment end-segment midpoint-segment)

(define (make-point x y) (cons x y))

(define (x-point p) (car p))

(define (y-point p) (cdr p))

(define (print-point p)
  (newline)
  (display "(")
  (display (x-point p))
  (display ",")
  (display (y-point p))
  (display ")"))

(define (make-segment p1 p2) (cons p1 p2))

(define (start-segment s) (car s))

(define (end-segment s) (cdr s))

(define (average x y)
  (/ (+ x y) 2))

(define (midpoint-segment s)
  (let ((start (start-segment s))
        (end (end-segment s)))
  (make-point (average (x-point start) (x-point end))
              (average (y-point start) (y-point end)))))