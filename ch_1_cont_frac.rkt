#lang racket
(provide cf cf-iter)

(define (cf n d k combiner)
  (define (helper cur)
    (if (= cur k)
        (/ (n cur) (d cur))
        (/ (n cur) (combiner (d cur) (helper (+ cur 1))))))
  (helper 1))

(define (cf-iter n d k combiner)
  (define (helper cur acc)
    (if (= cur 0)
        acc
        (helper (- cur 1) (/ (n cur) (combiner (d cur) acc)))))
  (helper k 0))