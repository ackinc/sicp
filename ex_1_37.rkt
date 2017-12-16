#lang racket
(require "./ch_1_cont_frac.rkt")

(define (cont-frac n d k) (cf n d k +))

(define (cont-frac-iter n d k) (cf-iter n d k +))

;TESTING
;approximating 1/phi using cont-frac
(define phi 1.6180327868852458)
(define i-phi (/ 1 phi))
(define (test-fn depth) (cont-frac (lambda (x) 1.0) (lambda (x) 1.0) depth))
(< (abs (- i-phi (test-fn 5))) 0.0001)
(< (abs (- i-phi (test-fn 6))) 0.0001)
(< (abs (- i-phi (test-fn 7))) 0.0001)
(< (abs (- i-phi (test-fn 8))) 0.0001)
(< (abs (- i-phi (test-fn 9))) 0.0001)
(< (abs (- i-phi (test-fn 10))) 0.0001) ; #t