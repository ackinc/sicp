#lang racket
(require "connector.rkt")
(require "constraints.rkt")

(define (cv val)
  (let ((z (make-connector)))
    (constant val z)
    z))

(define (c+ x y)
  (let ((z (make-connector)))
    (adder x y z)
    z))

(define (c* x y)
  (let ((z (make-connector)))
    (multiplier x y z)
    z))

(define (c- x y)
  (let ((z (make-connector)))
    (adder z y x)
    z))

(define (c/ x y)
  (let ((z (make-connector)))
    (multiplier z y x)
    z))

(define (celsius-to-fahrenheit-converter x)
  (c+ (c* (c/ (cv 9) (cv 5)) x)
      (cv 32)))

(define C (make-connector))
(define F (celsius-to-fahrenheit-converter C))

(probe 'C C)
(probe 'F F)

(set-value! F 212 'user)
(forget-value! F 'user)

(set-value! C 0 'user)
(forget-value! C 'user)