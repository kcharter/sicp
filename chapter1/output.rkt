#lang racket

(provide display-all display-line)

(define (display-all items)
  (foldl (lambda (item ignored) (display item) null) null items))

(define (display-line items)
  (display-all items)
  (newline))

