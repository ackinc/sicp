#lang racket
(define (exp b n)
  (if (= n 0) 1 (* b (exp b (- n 1)))))

(define (exp-iter b n)
  (define (helper counter acc)
    (if (= counter n) acc (helper (+ counter 1) (* b acc))))
  (helper 0 1))

(define (square x) (* x x))

(define (fast-exp b n)
  (cond ((= n 0) 1)
        ((even? n) (fast-exp (square b) (/ n 2)))
        (else (* b (fast-exp b (- n 1))))))

(define (fast-exp-iter b n)
  (define (helper b n a)
    (cond ((= n 0) a)
          ((even? n) (helper (square b) (/ n 2) a))
          (else (helper b (- n 1) (* a b)))))
  (helper b n 1))