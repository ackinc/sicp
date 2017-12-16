#lang racket
(define (compose f g)
  (lambda (x) (f (g x))))

(define (repeated f n)
  (if (= n 1)
      f
      (compose f (repeated f (- n 1)))))

(define (smooth f)
  (define dx 0.0001)
  (define (average x y z) (/ (+ x y z) 3))
  (lambda (x) (average (f (- x dx)) (f x) (f (+ x dx)))))

(define (multi-smooth f n)
  ((repeated smooth n) f))