#lang racket
(define (prime? n)
  (= (smallest-divisor n) n))

(define (square x) (* x x))

(define (smallest-divisor n)
  (define (try k)
    (cond ((> (square k) n) n)
          ((= (remainder n k) 0) k)
          (else (try (+ k 1)))))
  (try 2))

(smallest-divisor 199)
(smallest-divisor 1999)
(smallest-divisor 19999)