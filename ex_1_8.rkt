#lang racket
(define (abs x)
  (if (< x 0) (- x) x))

(define (square x) (* x x))

(define (cube x) (* x x x))

(define (good-enough? guess x)
  (< (abs (- x (cube guess))) 0.00000001))

(define (improve guess x)
  (/ (+ (/ x (square guess)) (* 2 guess)) 3))

(define (cbrt-iter x guess)
  (if (good-enough? guess x)
      guess
      (cbrt-iter x (improve guess x))))

(define (cbrt x)
  (cbrt-iter x 1.0))