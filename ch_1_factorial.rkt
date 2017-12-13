#lang racket
(define (fact n)
  (if (= n 0) 1 (* n (fact (- n 1)))))

(define (fact-iter n)
  (define (fact-iter-helper n result)
    (if (= n 0) result (fact-iter-helper (- n 1) (* result n))))
  (fact-iter-helper n 1))

(define (fact-iter-alt n)
  (define (fact-iter-helper counter product)
    (if (> counter n) product (fact-iter-helper (+ counter 1) (* product counter))))
  (fact-iter-helper 1 1))