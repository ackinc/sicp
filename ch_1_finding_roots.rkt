#lang racket
(provide half-interval)

(define (average x y) (/ (+ x y) 2))
(define (close-enough? x y) (< (abs (- x y)) 0.00001))
(define (cube x) (* x x x))

(define (search f neg pos)
  (let ((mid (average neg pos)))
    (if (close-enough? neg pos)
        mid
        (let ((test-val (f mid)))
          (cond ((negative? test-val) (search f mid pos))
                ((positive? test-val) (search f neg mid))
                (else mid))))))

(define (half-interval f a b)
  (let ((a-val (f a))
        (b-val (f b)))
    (cond ((and (<= a-val 0) (>= b-val 0)) (search f a b))
          ((and (>= a-val 0) (<= b-val 0)) (search f b a))
          (else (error "f(a) and f(b) are not on opposite sides of 0")))))

;TESTING
;(half-interval sin 2.0 4.0) ; pi
;(half-interval sin 0.0 1.0) ; 0
;(half-interval (lambda (x) (- (cube x) (* 2 x) 3)) 1.0 2.0) ; ~1.8933