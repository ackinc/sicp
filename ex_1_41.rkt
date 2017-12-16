#lang racket
(define (double f)
  (lambda (x) (f (f x))))

(define (inc x) (+ x 1))
(((double (double double)) inc) 5) ; 21

; (double double) returns (lambda (f) (double (double f)))
; (double (double double)) returns (lambda (f) (double (double (double (double f)))))