#lang racket
(require "./ch_2_sequence_ops.rkt")

(define (horner-eval x coefficient-sequence)
  (accumulate-alt (lambda (cur-coeff ans) (+ (* ans x) cur-coeff)) 0 coefficient-sequence))