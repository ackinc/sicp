#lang racket
(define (sqrt x)
  (define (abs x)
    (if (< x 0) (- x) x))
  
  (define (square x) (* x x))

  (define (average x y) (/ (+ x y) 2))
  
  (define (good-enough? guess)
    (< (abs (- x (square guess))) 0.00000001))
  
  (define (improve guess) (average guess (/ x guess)))
  
  (define (sqrt-iter guess)
    (if (good-enough? guess)
        guess
        (sqrt-iter (improve guess))))
  
  (sqrt-iter 1.0))