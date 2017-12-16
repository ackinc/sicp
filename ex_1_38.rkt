#lang racket
(require "./ch_1_cont_frac.rkt") ; cont-frac

(define (n x) 1.0)
(define (d x) (if (= (remainder x 3) 2) (+ 2 (* 2 (quotient x 3))) 1))
(define (cont-frac n d k) (cf n d k +))

(define (approx-e k) (+ 2 (cont-frac n d k)))

;TESTING
(approx-e 1)
(approx-e 5)
(approx-e 10)
(approx-e 20)
(approx-e 100)