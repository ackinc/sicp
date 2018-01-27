#lang racket
(provide fold-left fold-right)

(define (fold-left op initial sequence)
  (if (null? sequence)
      initial
      (fold-left op (op initial (car sequence)) (cdr sequence))))

(define (fold-right op initial sequence)
  (if (null? sequence)
      initial
      (op (car sequence) (fold-right op initial (cdr sequence)))))