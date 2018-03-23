#lang racket
(define (make-accumulator cur-val)
  (lambda (val)
    (set! cur-val (+ val cur-val))
    cur-val))

(define A1 (make-accumulator 1))
(= (A1 5) 6)
(= (A1 10) 16)


(define A2 (make-accumulator 10))
(= (A2 5) 15)
(= (A2 10) 25)