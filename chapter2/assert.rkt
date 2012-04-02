#lang racket

(provide assert-equal)

(define (assert-equal expected actual)
  (define (not-equal-error expected actual)
    (let ((sport (open-output-string)))
      (display "Expected " sport)
      (display expected sport)
      (display ", but got " sport)
      (display actual sport)
      (error (get-output-string sport))))
  (if (not (equal? expected actual))
      (not-equal-error expected actual)
      null))
