#lang racket
(require "./ch_1_cont_frac.rkt")

(define (cont-frac-alt n d k) (cf n d k -))

(define (tan-cf x k)
  (define (n k) (if (= k 1) x (* x x)))
  (define (d k) (- (* 2 k) 1))
  (cont-frac-alt n d k))

;TESTING
(tan-cf 0.785398 100) ; ~45 degs; should be close to 1
(tan-cf 1.5707 100) ; ~90 degs; should be a large positive no.
(tan-cf 3.14 100) ; ~180 degs; should be a negative no. close to 0