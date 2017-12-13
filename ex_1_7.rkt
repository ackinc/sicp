#lang racket
(define (abs x)
  (if (< x 0) (- x) x))

(define (square x) (* x x))

(define (average x y) (/ (+ x y) 2))

(define (good-enough? guess x)
  (< (abs (- guess (improve guess x))) (* 0.00000001 guess)))

(define (improve guess x) (average guess (/ x guess)))

(define (sqrt-iter x guess)
  (if (good-enough? guess x)
      guess
      (sqrt-iter x (improve guess x))))

(define (sqrt x)
  (sqrt-iter x 1.0))