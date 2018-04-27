#lang racket
(require "stream_procs.rkt")

(provide integral delayed-integral)

(define (integral integrand initial-value dt)
  (define int (cons-stream initial-value
                           (add-streams (scale-stream integrand dt) int)))
  int)

(define (delayed-integral delayed-integrand initial-value dt)
  (define int (cons-stream initial-value
                           (let ((integrand (force delayed-integrand)))
                             (add-streams (scale-stream integrand dt) int))))
  int)