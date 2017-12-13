#lang racket
(define (square x) (* x x))

(define (sum_squares a b)
  (+ (square a) (square b)))

(define (ss_largest a b c)
  (cond ((and (< a b) (< a c)) (sum_squares b c))
        ((and (< b a) (< b c)) (sum_squares a c))
        (#t (sum_squares a b))))