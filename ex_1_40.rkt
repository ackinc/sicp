#lang racket
(require "./ch_1_newton_method.rkt")

(define (cubic a b c)
  (define (square x) (* x x))
  (define (cube x) (* x x x))
  (lambda (x) (+ (cube x) (* a (square x)) (* b x) c)))

(newton-method (cubic 0 0 0)) ; 0
(newton-method (cubic 0 0 8)) ; -2
(newton-method (cubic 1 1 -100)) ; 4.2644