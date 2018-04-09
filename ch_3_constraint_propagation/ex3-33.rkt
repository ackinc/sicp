#lang racket
(require "connector.rkt")
(require "constraints.rkt")

(define a (make-connector))
(define b (make-connector))
(define s (make-connector))
(define p (make-connector))
(define avg (make-connector))

(adder a b s)
(multiplier a b p)
(averager a b avg)

(probe 'a a)
(probe 'b b)
(probe "SUM" s)
(probe "PRODUCT" p)
(probe "AVERAGE" avg)

(set-value! a 2 'user)
(set-value! b 5 'user)

(forget-value! a 'user)

(set-value! p 15 'user)