#lang racket
(define (square x) (* x x))
(define (inc x) (+ x 1))

(define (compose f g)
  (lambda (x) (f (g x))))

((compose square inc) 3) ; 16
((compose square inc) 6) ; 49