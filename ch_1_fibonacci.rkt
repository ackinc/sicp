#lang racket
(define (fib n)
  (cond ((= n 0) 0)
        ((= n 1) 1)
        (else (+ (fib (- n 1)) (fib (- n 2))))))

(define (fib-iter n)
  (define (helper a b counter)
    (if (= counter n) a (helper b (+ a b) (+ counter 1))))
  (helper 0 1 0))