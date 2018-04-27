#lang racket
(require "stream_procs.rkt")
(require (only-in "integral.rkt"
                  [delayed-integral integral]))

; solve eqns of the form <y'' = f(y', y)>
(define (solve dy0 y0 f dt)
  (define y (integral (delay dy) y0 dt))
  (define dy (integral (delay ddy) dy0 dt))
  (define ddy (stream-map f dy y))
  y)

(stream-ref (solve 1 1 (lambda (dy y) y) 0.001) 1000) ; e