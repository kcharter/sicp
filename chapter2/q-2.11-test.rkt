#lang racket/base

(require rackunit "q-2.11.rkt")

(require "q-2.7.rkt")

(define (mul-interval-orig x y)
  (let ((p1 (* (lower-bound x) (lower-bound y)))
        (p2 (* (lower-bound x) (upper-bound y)))
        (p3 (* (upper-bound x) (lower-bound y)))
        (p4 (* (upper-bound x) (upper-bound y))))
    (make-interval (min p1 p2 p3 p4)
                   (max p1 p2 p3 p4))))

       

(define b1 (make-interval -2 -1))
(define o1 (make-interval -2 1))
(define n1 (make-interval 1 2))

(define b2 (make-interval -7 -4))
(define o2 (make-interval -7 4))
(define n2 (make-interval 4 7))

(check-equal? (mul-interval-orig b1 b2)
              (mul-interval b1 b2)
              "b1 * b2")

(check-equal? (mul-interval-orig b1 o2)
              (mul-interval b1 o2)
              "b1 * o2")

(check-equal? (mul-interval-orig b1 n2)
              (mul-interval b1 n2)
              "b1 * n2")

(check-equal? (mul-interval-orig o1 b2)
              (mul-interval o1 b2)
              "o1 * b2")

(check-equal? (mul-interval-orig o1 o2)
              (mul-interval o1 o2)
              "o1 * o2")

(check-equal? (mul-interval-orig o1 n2)
              (mul-interval o1 n2)
              "o1 * n2")

(check-equal? (mul-interval-orig n1 b2)
              (mul-interval n1 b2)
              "n1 * b2")

(check-equal? (mul-interval-orig n1 o2)
              (mul-interval n1 o2)
              "n1 * o2")

(check-equal? (mul-interval-orig n1 n2)
              (mul-interval n1 n2)
              "n1 * n2")


