#lang racket
(require "stream_procs.rkt")
(require "ex_3_59.rkt")
(provide mul-series)

(define (mul-series s1 s2)
  (let ((s1car (stream-car s1))
        (s2car (stream-car s2)))
    (cons-stream (* s1car s2car)
                 (add-streams (cons-stream 0 (mul-series (stream-cdr s1) (stream-cdr s2)))
                              (add-streams (scale-stream (stream-cdr s1) s2car)
                                           (scale-stream (stream-cdr s2) s1car))))))

(define x (add-streams (mul-series sine-series sine-series) (mul-series cosine-series cosine-series)))
(eq? (stream-ref x 0) 1)
(eq? (stream-ref x 5) 0)
(eq? (stream-ref x 10) 0)
(eq? (stream-ref x 20) 0)