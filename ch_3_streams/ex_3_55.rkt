#lang racket
(require "stream_procs.rkt")
(provide partial-sums)

(define (partial-sums s)
  (define s0 (cons-stream (stream-car s) s0))
  (cons-stream (stream-car s) (add-streams s0 (partial-sums (stream-cdr s)))))

; alternative solution (more elegant)
(define (partial-sums-alt s)
  (add-streams s (cons-stream 0 (partial-sums-alt s))))

; TESTS
(define s (stream-enumerate-interval 1 10))
(define ps (partial-sums s))

(eq? (stream-ref ps 0) 1)
(eq? (stream-ref ps 3) 10)
(eq? (stream-ref ps 4) 15)
(eq? (stream-ref ps 5) 21)
