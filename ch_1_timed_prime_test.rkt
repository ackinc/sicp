#lang racket
(provide timed-prime-test)

(define (timed-prime-test prime? n)
  (newline)
  (display n)
  (start-prime-test prime? n (current-milliseconds)))

(define (start-prime-test prime? n start-time)
  (if (prime? n) (report-prime (- (current-milliseconds) start-time)) (display "")))

(define (report-prime elapsed-time)
  (display " *** ")
  (display elapsed-time))