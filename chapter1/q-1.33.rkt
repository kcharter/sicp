#lang racket

(define (filtered-accumulate combiner null-value filter term a next b)
  (define (go x r)
    (if (> x b)
        r
        (let ((t (term x)))
          (go (next x) (if (filter t)
                           (combiner r t)
                           r)))))
  (go a null-value))

;; TODO: need to write or find a primality tester and a relative
;; primality tester to answer the application parts of the question
              