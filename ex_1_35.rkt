#lang racket
(require "./ch_1_fixed_point.rkt")

(define phi (fixed-point (lambda (x) (+ 1 (/ 1 x))) 1.0))
phi ; ~1.618