#lang racket

;; a representation of rectangles in the plane

(require "q-2.2.rkt")
(require "assert.rkt")

(provide make-rectangle
         lower-left-rectangle
         upper-right-rectangle
         upper-left-rectangle
         lower-right-rectangle
         width-rectangle
         height-rectangle
         perimeter-rectangle
         area-rectangle
         make-rectangle-1
         lower-left-rectangle-1
         upper-right-rectangle-1
         width-rectangle-1
         height-rectangle-1
         perimeter-rectangle-1
         area-rectangle-1)


;; the make-rectangle, lower-left-rectangle, and
;; upper-right-rectangle functions are fundamental, though the
;; upper-left-rectangle, lower-right-rectangle, width-rectangle and
;; length-rectangle functions could also be re-implemented with a
;; different primitive representation

(define (make-rectangle p1 p2)
  (let ((x1 (x-point p1))
        (y1 (y-point p1))
        (x2 (x-point p2))
        (y2 (y-point p2)))
    (let ((minx (min x1 x2))
          (miny (min y1 y2))
          (maxx (max x1 x2))
          (maxy (max y1 y2)))
      (let ((lower-left (make-point minx miny))
            (upper-right (make-point maxx maxy)))
        (cons lower-left upper-right)))))

(define (lower-left-rectangle r)
  (car r))

(define (upper-right-rectangle r)
  (cdr r))

(define (upper-left-rectangle r)
  (make-point (x-point (lower-left-rectangle r))
              (y-point (upper-right-rectangle r))))

(define (lower-right-rectangle r)
  (make-point (x-point (upper-right-rectangle r))
              (y-point (lower-left-rectangle r))))

(define (width-rectangle r)
  (- (x-point (upper-right-rectangle r)) (x-point (lower-left-rectangle r))))

(define (height-rectangle r)
  (- (y-point (upper-right-rectangle r)) (y-point (lower-left-rectangle r))))

;;  the perimeter-rectangle and area-rectangle functions need not be
;;  redefined if there is a change of representation

(define (perimeter-rectangle r)
  (* 2 (+ (width-rectangle r) (height-rectangle r))))

(define (area-rectangle r)
  (* (width-rectangle r) (height-rectangle r)))

;; an alternative representation of rectangles would store perhaps a
;; single anchor point, the lower-left say, along with the width and
;; height.

(define (make-rectangle-1 lower-left width height)
  (cons lower-left (cons width height)))

(define (lower-left-rectangle-1 r)
  (car r))

;; the other functions can remain unchanged, although in the interests
;; of efficiency we would probably want to override the width and
;; height functions
(define (upper-right-rectangle-1 r)
  (let ((ul (car r))
        (w (cadr r))
        (h (cddr r)))
    (make-point (+ (x-point ul) w) (+ (y-point ul) h))))

;; the width, height, perimeter, and area changes are unchanged except
;; for the names to avoid clashes above. If we didn't have the '-1'
;; suffix, the code would be identical. For width and height, we'd
;; probably want to change the functions for efficiency.

(define (width-rectangle-1 r)
  (- (x-point (upper-right-rectangle-1 r)) (x-point (lower-left-rectangle-1 r))))

(define (height-rectangle-1 r)
  (- (y-point (upper-right-rectangle-1 r)) (y-point (lower-left-rectangle-1 r))))

(define (perimeter-rectangle-1 r)
  (* 2 (+ (width-rectangle-1 r) (height-rectangle-1 r))))

(define (area-rectangle-1 r)
  (* (width-rectangle-1 r) (height-rectangle-1 r)))

;; here is a small demo

(define p1 (make-point 1 1))

(define p2 (make-point 10 6))

(define r (make-rectangle p1 p2))

(define r1 (make-rectangle p2 p1))

(define r2 (make-rectangle-1 p1 (width-rectangle r) (height-rectangle r)))

(assert-equal (lower-left-rectangle r) (lower-left-rectangle r1))
(assert-equal (upper-left-rectangle r) (upper-left-rectangle r1))
(assert-equal (lower-right-rectangle r) (lower-right-rectangle r1))
(assert-equal (upper-right-rectangle r) (upper-right-rectangle r1))

(assert-equal (perimeter-rectangle r) (perimeter-rectangle r1))
(assert-equal (area-rectangle r) (area-rectangle r1))

(assert-equal (lower-left-rectangle r) (lower-left-rectangle-1 r2))
(assert-equal (upper-right-rectangle r) (upper-right-rectangle-1 r2))
(assert-equal (width-rectangle r) (width-rectangle-1 r2))
(assert-equal (height-rectangle r) (height-rectangle-1 r2))
(assert-equal (perimeter-rectangle r) (perimeter-rectangle-1 r2))
(assert-equal (area-rectangle r) (area-rectangle-1 r2))
