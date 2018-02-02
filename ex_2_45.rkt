#lang racket
(define (split tr1 tr2)
  (define (helper painter n)
    (if (= n 0)
        painter
        (let ((smaller (helper painter (- n 1))))
          (tr1 painter (tr2 smaller smaller)))))
  helper)