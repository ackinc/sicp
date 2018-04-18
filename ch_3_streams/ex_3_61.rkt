#lang racket
(require "stream_procs.rkt")
(require "ex_3_54.rkt")
(require "ex_3_60.rkt")

(provide invert-unit-series)

(define (invert-unit-series s)
  (define inverted-series (cons-stream 1 (scale-stream (mul-series (stream-cdr s) inverted-series) -1)))
  inverted-series)

; TESTS
(define s1 (invert-unit-series integers))
(define s2 (mul-series s1 integers))
(eq? (stream-ref s2 0) 1)
(eq? (stream-ref s2 5) 0)
(eq? (stream-ref s2 10) 0)
(eq? (stream-ref s2 20) 0)