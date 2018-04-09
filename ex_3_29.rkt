#lang racket
(define (or-gate a1 a2 output)
  (let ((a1-out (make-wire))
        (a2-out (make-wire))
        (a3 (make-wire)))
    (inverter a1 a1-out)
    (inverter a2 a2-out)
    (and-gate a1-out a2-out a3)
    (inverter a3 output)))