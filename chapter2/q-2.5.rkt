#lang racket

(provide cons car cdr)

;; (A) Show that we can represnt pairs of nonnegative integers using only
;; numbers and arithmetic operations if we represent the pair a and b
;; as the integer that is the product 2^a*3^b. Give the corresponding
;; definitions of the procedures cons, car and cdr.

;; (Q) Note that 2 and 3 are distinct prime numbers. Therefore:

;; - 'a' is the number of times that we can divide 2^a*3^b by 2 with a
;; zero remainder

;; - 'b' is the number of times that we can divide 2^a*3^b by 3 with a
;; zero remainder

;; - since 2 and 3 are the only prime factors of 2^a*3^b, the value of
;; 2^a*3^b is uniquely determined by 'a' and 'b'b.

;; raises an integer 'n' to the 'mth' power
(define (pow n m)
  (define (iter p sofar)
    (if (= p 0)
        sofar
        (iter (- p 1) (* sofar n))))
  (iter m 1))

;; counts the number of factors of a prime number 'p' in an integer 'n'
(define (count-factors p n)
  (define (iter m sofar)
    (if (not (= 0 (remainder m p)))
        sofar
        (iter (quotient m p) (+ sofar 1))))
  (iter n 0))

;; so cons is the product 2^a*3^b
(define (cons a b)
  (* (pow 2 a) (pow 3 b)))

;; and 'a' is the number of factors of 2
(define (car z)
  (count-factors 2 z))

;; and 'b' is the number of factors of 3
(define (cdr z)
  (count-factors 3 z))


