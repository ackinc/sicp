#lang racket
(define (count-change n)
  (define (max-denom n)
    (cond ((>= n 50) 50)
          ((>= n 25) 25)
          ((>= n 10) 10)
          ((>= n 5) 5)
          (else 1)))

  (define (next-denom curdenom)
    (cond ((= curdenom 50) 25)
          ((= curdenom 25) 10)
          ((= curdenom 10) 5)
          ((= curdenom 5) 1)
          (else 0)))
  
  (define (cc-helper n maxcoin)
    (cond ((< n 0) 0)
          ((= n 0) 1)
          (else (+ (cc-helper (- n maxcoin) maxcoin)
                   (if (= maxcoin 1) 0 (cc-helper n (next-denom maxcoin)))))))

  (cc-helper n (max-denom n)))