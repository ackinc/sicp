#lang racket
(require "./ch_1_prime.rkt")

(define (filtered-accumulate predfn? combiner null-value term a next b)
  (cond ((> a b) null-value)
        (else (combiner (if (predfn? a) (term a) null-value)
                        (filtered-accumulate predfn? combiner null-value term (next a) next b)))))

(define (square x) (* x x))
(define (increment x) (+ x 1))
(define (sum-squares-prime a b)
  (filtered-accumulate prime? + 0 square a increment b))
(sum-squares-prime 2 10) ; 2^2 + 3^2 + 5^2 + 7^2 = 87

(define (gcd a b)
  (if (= b 0) a (gcd b (remainder a b))))
(define (prod-rel-prime n)
  (define (rel-prime x) (= (gcd x n) 1))
  (filtered-accumulate rel-prime * 1 identity 1 increment n))
(prod-rel-prime 10) ; 3 * 7 * 9 = 189