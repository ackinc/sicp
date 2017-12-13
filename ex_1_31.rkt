#lang racket
(define (product term a next b)
  (if (> a b)
      1
      (* (term a) (product term (next a) next b))))

(define (product-iter term a next b)
  (define (iter cur acc)
    (if (> cur b)
        acc
        (iter (next cur) (* (term cur) acc))))
  (iter a 1))

; factorial
(define (increment n) (+ n 1))
(define (factorial n)
  (product identity 1 increment n))

(factorial 0)
(factorial 1)
(factorial 3)
(factorial 5)
(factorial 6)

; pi-prod
(define (pi-prod b)
  (define (term a) (/ (* (- a 1) (+ a 1)) (* a a)))
  (define (next a) (+ a 2))
  (product term 3 next b))

(* 4.0 (pi-prod 1000))
(* 4.0 (pi-prod 10000))
