#lang racket
(require "./ch_1_fixed_point.rkt")

; no avg damping
(fixed-point-debug (lambda (x) (/ (log 1000) (log x))) 2)

(newline)
(newline)

; with avg damping
(define (average x y) (/ (+ x y) 2))
(fixed-point-debug (lambda (x) (average x (/ (log 1000) (log x)))) 2)