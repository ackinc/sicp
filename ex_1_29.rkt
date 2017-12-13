#lang racket
(require "./ch_1_sum.rkt")

(define (simpsons-integral f a b n)
  (define h (/ (- b a) n))
  (define (next a) (+ a (* 2 h)))
  (define (term a)
    (define (y k) (f (+ a (* k h))))
    (+ (y 0) (* 4 (y 1)) (y 2)))
  (* (/ h 3) (sum term a next b)))

(define (cube x) (* x x x))

(simpsons-integral cube 0 1.0 100)
(simpsons-integral cube 0 1.0 1000)