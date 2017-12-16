#lang racket
(provide newton-method)

(require "./ch_1_fixed_point.rkt")

(define (deriv g)
  (let ((dx 0.00001))
    (lambda (x) (/ (- (g (+ x dx)) (g x)) dx))))

(define (newton-transform g)
  (lambda (x) (- x (/ (g x) ((deriv g) x)))))

(define (newton-method g)
  (fixed-point (newton-transform g) 1.0))