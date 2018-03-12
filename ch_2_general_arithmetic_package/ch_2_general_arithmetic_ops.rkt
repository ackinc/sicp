#lang racket
(require "ch_2_apply_generic_op.rkt")
(require "ch_2_hash_ops.rkt")
(require "ch_2_type_tag_helpers.rkt")

(provide add sub mul div equ? =zero?
         sine cosine arctan
         squareroot)

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
  (let ((proc (get '=zero? (type-tag x))))
    (if proc
        (proc (contents x))
        (error "=zero? -- no procedure found for this argument" x))))

; ex 2.86
(define (sine x)
  (let ((proc (get 'sin (type-tag x))))
    (if proc
        (proc (contents x))
        (error "sine -- no procedure found for this argument" x))))

(define (cosine x)
  (let ((proc (get 'cos (type-tag x))))
    (if proc
        (proc (contents x))
        (error "cosine -- no procedure found for this argument" x))))

(define (arctan y x)
  (apply-generic-op 'atan y x))

(define (squareroot x)
  (let ((proc (get 'sqrt (type-tag x))))
    (if proc
        (proc (contents x))
        (error "squareroot -- no procedure found for this argument" x))))