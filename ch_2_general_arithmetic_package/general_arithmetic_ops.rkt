#lang racket
(require "apply_generic.rkt")
(require "hash_ops.rkt")
(require "type_tower.rkt")

(provide add sub mul div
         equ? =zero?
         raise project
         sine cosine arctan
         squareroot
         negate)

(define (add x y) (apply-generic 'add x y))

(define (sub x y) (apply-generic 'sub x y))

(define (mul x y) (apply-generic 'mul x y))

(define (div x y) (apply-generic 'div x y))

(define (equ? x y) (apply-generic 'equ? x y))

(define (=zero? x) (apply-generic '=zero? x))

; ex 2.83
(define (raise x) (apply-generic 'raise x))

; ex 2.85
(define (project x) (apply-generic 'project x))

(define (drop x)
  (if (is-lowest-type x)
      x
      (let* ((x-p (project x))
             (x-rp (raise x-p)))
        (if (equ? x x-rp) (drop x-p) x))))

; ex 2.86
(define (sine x) (apply-generic 'sin x))

(define (cosine x) (apply-generic 'cos x))

(define (arctan y x) (apply-generic 'atan y x))

(define (squareroot x) (apply-generic 'sqrt x))

; ex 2.88
(define (negate x) (apply-generic 'negate x))