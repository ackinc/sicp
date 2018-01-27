#lang racket
(require (only-in "./ch_2_sequence_ops.rkt"
                  [accumulate-alt accumulate]))
(require "./ex_2_36.rkt") ; accumulate-n

(define (dot-product v w)
  (accumulate + 0 (map * v w)))

(define (matrix-*-vector m v)
  (map (lambda (w) (dot-product v w)) m))

(define (transpose m)
  (accumulate-n cons '() m))

(define (matrix-*-matrix m n)
  (let ((cols (transpose n)))
    (map (lambda (v) (matrix-*-vector cols v)) m)))