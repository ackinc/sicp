#lang racket
(define (accumulate combiner null-value term a next b)
  (if (> a b)
      null-value
      (combiner (term a) (accumulate combiner null-value term (next a) next b))))

(define (accumulate-iter combiner null-value term a next b)
  (define (iter cur acc)
    (if (> cur b)
        acc
        (iter (next cur) (combiner (term cur) acc))))
  (iter a null-value))

(define (sum term a next b) (accumulate + 0 term a next b))
(define (product term a next b) (accumulate * 1 term a next b))

; simpson's integral of cube
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

; pi-prod
(define (pi-prod b)
  (define (term a) (/ (* (- a 1) (+ a 1)) (* a a)))
  (define (next a) (+ a 2))
  (product term 3 next b))

(* 4.0 (pi-prod 1000))
(* 4.0 (pi-prod 10000))