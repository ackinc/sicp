#lang racket
(require "scheme_numbers.package.rkt")
(require "rational_numbers.package.rkt")
(require "complex_numbers.package.rkt")
(require "polynomials.package.rkt")
(require "general_arithmetic_ops.rkt")

(require "hash_ops.rkt")

(define p1 (make-polynomial 'x '((2 1) (0 1))))
(define p2 (make-polynomial 'x '((3 1) (0 1))))
(add p1 p2)
;(define rf (make-rational p2 p1))
;(add rf rf)