#lang racket
(require "type_tag_helpers.rkt")
(require "hash_ops.rkt")
(require "general_arithmetic_ops.rkt")

(define (square x) (mul x x))
(define (install-rectangular-complex-number-package)
  (define (real-part z) (car z))
  (define (imag-part z) (cadr z))
  (define (magnitude z) (squareroot (add (square (real-part z)) (square (imag-part z)))))
  (define (angle z) (arctan (imag-part z) (real-part z)))

  (define (make-from-real-imag x y) (list x y))
  (define (make-from-mag-ang r a) (list (mul r (cosine a)) (mul r (sine a))))

  (define (negate-rect z) (make-from-real-imag (negate (real-part z)) (negate (imag-part z))))

  (define (tag datum) (attach-tag 'rect datum))
  (put 'make-from-real-imag 'rect (lambda (x y) (tag (make-from-real-imag x y))))
  (put 'make-from-mag-ang 'rect (lambda (r a) (tag (make-from-mag-ang r a))))
  (put 'negate 'rect (lambda (z) (tag (make-from-real-imag (negate (real-part z)) (negate (imag-part z))))))
  (put 'real-part '(rect) real-part)
  (put 'imag-part '(rect) imag-part)
  (put 'magnitude '(rect) magnitude)
  (put 'angle '(rect) angle)
  (put 'negate '(rect) (lambda (z) (tag (negate-rect z))))
  'installed-rectangular-complex-numbers-package)

(install-rectangular-complex-number-package)