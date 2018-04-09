#lang racket
(require "agenda.rkt")
(provide after-delay inverter-delay and-gate-delay or-gate-delay)

(define the-agenda (make-agenda))
(define inverter-delay 2)
(define and-gate-delay 3)
(define or-gate-delay 5)

(define (after-delay delay action)
  (add-to-agenda! (+ (current-time the-agenda) delay) action the-agenda))

(define (propagate)
  (if (empty-agenda? the-agenda)
      'ok
      (begin ((first-agenda-item the-agenda))
             (remove-first-agenda-item! the-agenda)
             (propagate))))