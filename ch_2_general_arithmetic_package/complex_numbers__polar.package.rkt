#lang racket
(require "type_tag_helpers.rkt")
(require "hash_ops.rkt")
(require "general_arithmetic_ops.rkt")

(define (install-polar-complex-number-package)
  ; implementation of public functions
  (define (magnitude z) (car z))
  (define (angle z) (cadr z))
  (define (real-part z) (mul (magnitude z) (cosine (angle z))))
  (define (imag-part z) (mul (magnitude z) (sine (angle z))))

  (define (make-from-mag-ang r a) (list r a))
  (define (make-from-real-imag x y) (list (squareroot (add (mul x x) (mul y y))) (arctan y x))) ; sqrt

  (define (negate-polar z) (make-from-mag-ang (magnitude z) (+ (angle z) pi)))

  ; public interface
  (define (tag datum) (attach-tag 'polar datum))
  (put 'make-from-mag-ang 'polar (lambda (r a) (tag (make-from-mag-ang r a))))
  (put 'make-from-real-imag 'polar (lambda (x y) (tag (make-from-real-imag x y))))
  (put 'negate 'polar (lambda (z) (tag (make-from-mag-ang (magnitude z) (add (angle z) pi)))))
  (put 'real-part '(polar) real-part)
  (put 'imag-part '(polar) imag-part)
  (put 'magnitude '(polar) magnitude)
  (put 'angle '(polar) angle)
  (put 'negate '(polar) (lambda (z) (tag (negate-polar z))))
  'installed-polar-complex-numbers-package)

(install-polar-complex-number-package)