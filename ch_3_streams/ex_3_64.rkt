#lang racket
(require "stream_procs.rkt")

(define (stream-limit s tolerance)
  (define (helper stream prev)
    (if (< (abs (- (stream-car stream) prev)) tolerance)
        (stream-car stream)
        (helper (stream-cdr stream) (stream-car stream))))
  (helper (stream-cdr s) (stream-car s)))

; TESTING
(define (sqrt-stream x)
  (define (improve-guess guess)
    (/ (+ guess (/ x guess)) 2.0))
  (define guesses (cons-stream 1 (stream-map improve-guess guesses)))
  guesses)

(define (sqrt x tolerance)
  (stream-limit (sqrt-stream x) tolerance))