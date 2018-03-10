#lang racket
(require "ch_2_type_tag_helpers.rkt")
(require "ch_2_apply_generic_op.rkt")
(require "ch_2_hash_ops.rkt")
(require "ch_2_scheme_number_package.rkt")
(require "ch_2_rational_number_package.rkt")
(require "ch_2_complex_number_package.rkt")

(provide add sub mul div equ? =zero?)

(define (add x y)
  (apply-generic-op 'add x y))

(define (sub x y)
  (apply-generic-op 'sub x y))

(define (mul x y)
  (apply-generic-op 'mul x y))

(define (div x y)
  (apply-generic-op 'div x y))

(define (equ? x y)
  (apply-generic-op 'equ? x y))

(define (=zero? x)
  ((get '=zero? (type-tag x)) (contents x)))