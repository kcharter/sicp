#lang racket

(require "powers.rkt")

(provide cubic)

(define (cubic a b c)
  (lambda (x)
    (+ (cube x) (* a (square x)) (* b x) c)))

