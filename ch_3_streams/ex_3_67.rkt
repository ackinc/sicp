#lang racket
(require "stream_procs.rkt")
(provide pairs-orig pairs)

(define (pairs-orig s t)
  (cons-stream (list (stream-car s) (stream-car t))
               (interleave (stream-map (lambda (x) (list (stream-car s) x)) (stream-cdr t))
                           (pairs-orig (stream-cdr s) (stream-cdr t)))))

(define (pairs s t)
  (cons-stream (list (stream-car s) (stream-car t))
               (interleave (interleave (stream-map (lambda (x) (list (stream-car s) x)) (stream-cdr t))
                                       (stream-map (lambda (x) (list x (stream-car t))) (stream-cdr s)))
                           (pairs (stream-cdr s) (stream-cdr t)))))

; TESTS
(define ones (cons-stream 1 ones))
(define integers (cons-stream 1 (add-streams ones integers)))
(define s (pairs integers integers))
;(display-stream s 10)