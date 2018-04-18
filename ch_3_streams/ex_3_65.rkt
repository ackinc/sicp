#lang racket
(require "stream_procs.rkt")
(require "sequence_acceleration.rkt")
(require "ex_3_55.rkt") ; partial-sums

(define (s-ln2 n) (cons-stream (/ 1 n) (scale-stream (s-ln2 (+ n 1)) -1)))

; first approx
(define a (partial-sums (s-ln2 1)))
(newline)(display "First approx")(newline)
(display-stream (stream-map (lambda (x) (abs (- (log 2) x))) a) 5)

; second approx
(define b (euler-transform a))
(newline)(display "Second approx")(newline)
(display-stream (stream-map (lambda (x) (abs (- (log 2) x))) b) 5)

; third approx
(define c (accelerate-sequence a))
(newline)(display "Third approx")(newline)
(display-stream (stream-map (lambda (x) (abs (- (log 2) x))) c) 5)
