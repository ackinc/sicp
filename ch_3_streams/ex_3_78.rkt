#lang racket
(require "stream_procs.rkt")
(require (only-in "integral.rkt"
                  [delayed-integral integral]))

; solve eqns of the form <y'' = ay' + by>
(define (solve dy0 y0 a b dt)
  (define y (integral (delay dy) y0 dt))
  (define dy (integral (delay ddy) dy0 dt))
  (define ddy (add-streams (scale-stream dy a) (scale-stream y b)))
  y)