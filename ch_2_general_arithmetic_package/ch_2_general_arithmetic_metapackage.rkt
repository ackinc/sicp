#lang racket
(require "scheme_numbers.package.rkt")
(require "rational_numbers.package.rkt")
(require "complex_numbers.package.rkt")
(require "polynomials.package.rkt")
(require "general_arithmetic_ops.rkt")

(require "hash_ops.rkt")

; ex 2.93
(define p31 (make-polynomial 'x '((2 1) (0 1))))
(define p32 (make-polynomial 'x '((3 1) (0 1))))
(define rf (make-rational p32 p31))
(add rf rf)

; ex 2.94
(define p41 (make-polynomial 'x '((4 1) (3 -1) (2 -2) (1 2))))
(define p42 (make-polynomial 'x '((3 1) (1 -1))))
(greatest-common-divisor p41 p42)

; ex 2.95
(define P1 (make-polynomial 'x '(1 -2 1)))
(define P2 (make-polynomial 'x '(11 0 7)))
(define P3 (make-polynomial 'x '(13 5)))
(define Q1 (mul P1 P2))
(define Q2 (mul P1 P3))
(greatest-common-divisor Q1 Q2)

; ex 2.97
(define p71 (make-polynomial 'x '((1 1) (0 1))))
(define p72 (make-polynomial 'x '((3 1) (0 -1))))
(define p73 (make-polynomial 'x '((1 1))))
(define p74 (make-polynomial 'x '((2 1) (0 -1))))
(define r71 (make-rational p71 p72))
(define r72 (make-rational p73 p74))
(add r71 r72)