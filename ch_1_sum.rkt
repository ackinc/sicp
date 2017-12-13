#lang racket
(provide sum sum-integers sum-cubes pi-sum integrate)

(define (identity x) x)
(define (increment x) (+ x 1))
(define (cube x) (* x x x))

(define (sum term a next b)
  (if (> a b) 0 (+ (term a) (sum term (next a) next b))))

(define (sum-integers a b) (sum identity a increment b))

(define (sum-cubes a b) (sum cube a increment b))

(define (pi-sum b)
  (define (pi-next a) (+ a 4))
  (define (pi-term a) (/ 1 (* a (+ a 2))))
  (sum pi-term 1 pi-next b))

(define (integrate f a b dx)
  (define (next a) (+ a dx))
  (* dx (sum f (+ a (/ dx 2)) next b)))