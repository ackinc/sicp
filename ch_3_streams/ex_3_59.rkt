#lang racket
(require "stream_procs.rkt")
(require "ex_3_54.rkt")

(provide integrate-series sine-series cosine-series)

;a
(define (integrate-series series)
  ; first try
  ;     (x (stream-map (lambda (x) (/ 1 x)) integers)))
  ;(mul-streams x series)))

  ; most elegant soln
  (stream-map / series integers))

;b
(define cosine-series (cons-stream 1 (scale-stream (integrate-series sine-series) -1)))
(define sine-series (cons-stream 0 (integrate-series cosine-series)))