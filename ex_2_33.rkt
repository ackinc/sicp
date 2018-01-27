#lang racket
(require "./ch_2_sequence_ops.rkt")

(define (map p seq)
  (accumulate-alt (lambda (x y) (cons (p x) y)) null seq))

(define (filter pred? seq)
  (accumulate-alt (lambda (x y) (if (pred? x) (cons x y) y)) null seq))

(define (append s1 s2)
  (accumulate-alt cons s2 s1))

(define (length seq)
  (accumulate-alt (lambda (x y) (+ y 1)) 0 seq))