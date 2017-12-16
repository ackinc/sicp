#lang racket
(define (iterative-improve good-enough? improve)
  (define (helper guess) (if (good-enough? guess) guess (helper (improve guess))))
  helper)

(define (sqrt x)
  ((iterative-improve (lambda (y) (< (abs (- (* y y) x)) 0.0001))
                      (lambda (y) (/ (+ y (/ x y)) 2.0)))
   x))

(sqrt 16) ; 4


(define (fixed-point f first-guess)
  ((iterative-improve (lambda (y) (< (abs (- (f y) y)) 0.0001))
                      (lambda (y) (f y)))
   first-guess))

(fixed-point (lambda (x) (/ (log 1000) (log x))) 2.0) ; 4.555
(fixed-point (lambda (x) (+ 1 (/ 1 x))) 1.0) ; 1.6179 (golden-ratio)