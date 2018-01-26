#lang racket
(define (cc amount coins)
  (cond ((= amount 0) 1)
        ((or (< amount 0) (null? coins)) 0)
        (else (+ (cc amount (cdr coins))
                 (cc (- amount (car coins)) coins)))))