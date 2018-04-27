#lang racket
(require "./ch_3_monte_carlo.rkt")

(define (random-in-range low high) (+ low (* (random) (- high low))))

(define (estimate-integral P x1 x2 y1 y2 trials)
  (define (experiment)
    (let ((x (random-in-range x1 x2))
          (y (random-in-range y1 y2)))
      (P x y)))
  (let ((area (abs (* (- x2 x1) (- y2 y1)))))
    (* area (monte-carlo trials experiment))))

; estimating area of unit circle
(define (square x) (* x x))
(define (in-unit-circle? x y) (<= (+ (square x) (square y)) 1))
(define (estimate-pi trials) (estimate-integral in-unit-circle? -1.0 1.0 -1.0 1.0 trials))
(estimate-pi 20000)