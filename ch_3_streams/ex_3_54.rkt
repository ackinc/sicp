#lang racket
(require "stream_procs.rkt")
(provide ones integers factorials)

(define ones (cons-stream 1 ones))
(define integers (cons-stream 1 (add-streams ones integers)))

(define factorials (cons-stream 1 (mul-streams (add-streams ones integers) factorials)))

; TESTS
(eq? (stream-ref ones 0) 1)
(eq? (stream-ref ones 10) 1)
(eq? (stream-ref ones 100) 1)

(eq? (stream-ref integers 0) 1)
(eq? (stream-ref integers 10) 11)
(eq? (stream-ref integers 100) 101)

(eq? (stream-ref factorials 0) 1)
(eq? (stream-ref factorials 1) 2)
(eq? (stream-ref factorials 2) 6)
(eq? (stream-ref factorials 4) 120)
