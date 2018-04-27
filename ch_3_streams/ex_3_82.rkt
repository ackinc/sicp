#lang racket
(require "stream_procs.rkt")

(define (random-in-range low high) (+ low (* (random) (- high low))))

(define (monte-carlo experiment)
  (define (helper past-successes past-trials)
    (if (experiment)
        (cons-stream (/ (+ 1 past-successes) (+ 1 past-trials)) (helper (+ 1 past-successes) (+ 1 past-trials)))
        (cons-stream (/ past-successes (+ 1 past-trials)) (helper past-successes (+ 1 past-trials)))))
  (helper 0.0 0.0))

(define (estimate-integral P x1 x2 y1 y2)
  (define (experiment)
    (let ((x (random-in-range x1 x2))
          (y (random-in-range y1 y2)))
      (P x y)))
  (let ((area (abs (* (- x2 x1) (- y2 y1)))))
    (scale-stream (monte-carlo experiment) area)))

; estimating pi from area of unit circle
(define (square x) (* x x))
(define (in-unit-circle? x y) (<= (+ (square x) (square y)) 1))
(define pi-est (estimate-integral in-unit-circle? 1 -1 1 -1))
(display-stream pi-est 100000)