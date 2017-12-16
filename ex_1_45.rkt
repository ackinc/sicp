#lang racket
(require "./ch_1_fixed_point.rkt")
(require "./ch_1_average_damp.rkt")

;HELPERS
(define (compose f g) (lambda (x) (f (g x))))
(define (repeated f n) (if (= n 1) f (compose f (repeated f (- n 1)))))
(define (log2 x) (/ (log x) (log 2)))
(define (power base exp)
  (cond ((= exp 0) 1)
        ((odd? exp) (* base (power base (- exp 1))))
        (else (power (* base base) (/ exp 2)))))

(define (nth-root x n)
  (fixed-point ((repeated average-damp (floor (log2 n))) (lambda (y) (/ x (power y (- n 1))))) 1.0))
