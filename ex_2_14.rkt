#lang racket
(require "./ch_2_interval_arithmetic.rkt")

(define a (make-interval-cp 3.0 1))
(define b (make-interval-cp 4.0 2))

(define (par-interval i1 i2)
  (div-interval (mul-interval i1 i2) (add-interval i1 i2)))

(define (par-interval2 i1 i2)
  (let ((i (make-interval 1 1)))
    (div-interval i (add-interval (div-interval i i1) (div-interval i i2)))))

; pretty unequal!
(par-interval a b)
(par-interval2 a b)