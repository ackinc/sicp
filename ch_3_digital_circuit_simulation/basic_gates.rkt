#lang racket
(require "wires.rkt")
(require "main.rkt")
(provide inverter and-gate or-gate)

(define (inverter input output)
  (define (not-gate-proc)
    (let ((new-value (logical-not (get-signal input))))
      (after-delay inverter-delay (lambda () (set-signal! output new-value)))))
  (add-action! input not-gate-proc))

(define (and-gate a1 a2 output)
  (define (and-gate-proc)
    (let ((new-val (logical-and (get-signal a1) (get-signal a2))))
      (after-delay and-gate-delay (lambda () (set-signal! output new-val)))))
  (add-action! a1 and-gate-proc)
  (add-action! a2 and-gate-proc))

; ex 3.28
(define (or-gate a1 a2 output)
  (define (or-gate-proc)
    (let ((new-val (logical-or (get-signal a1) (get-signal a2))))
      (after-delay or-gate-delay (lambda () (set-signal! output new-val)))))
  (add-action! a1 or-gate-proc)
  (add-action! a2 or-gate-proc))

(define (logical-not x)
  (if (= x 1) 0 1))

(define (logical-and x y)
  (if (and (= x 1) (= y 1)) 1 0))

(define (logical-or x y)
  (if (or (= x 1) (= y 1)) 1 0))