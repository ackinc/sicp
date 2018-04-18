#lang racket
(require "stream_procs.rkt")

(provide euler-transform accelerate-sequence)

(define (square x) (* x x))

(define (euler-transform s)
  (let ((s0 (stream-ref s 0))
        (s1 (stream-ref s 1))
        (s2 (stream-ref s 2)))
    (cons-stream (- s2 (/ (square (- s2 s1)) (- (+ s0 s2) (* 2 s1)))) (euler-transform (stream-cdr s)))))

(define (make-tableau transform s)
  (cons-stream s (make-tableau transform (transform s))))

(define (accelerate-sequence s)
  (stream-map stream-car (make-tableau euler-transform s)))