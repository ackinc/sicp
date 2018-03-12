#lang racket
(require "ch_2_type_tag_helpers.rkt")
(require "ch_2_hash_ops.rkt")
(require "ch_2_general_arithmetic_ops.rkt")

(define (square x) (mul x x))
(define (install-rectangular-complex-number-package)
  (define (real-part z) (car z))
  (define (imag-part z) (cdr z))
  (define (magnitude z) (squareroot (add (square (real-part z)) (square (imag-part z)))))
  (define (angle z) (arctan (imag-part z) (real-part z)))

  (define (make-from-real-imag x y) (cons x y))
  (define (make-from-mag-ang r a) (cons (mul r (cosine a)) (mul r (sine a))))

  (define (tag datum) (attach-tag 'rect datum))
  (put 'make-from-real-imag 'rect (lambda (x y) (tag (make-from-real-imag x y))))
  (put 'make-from-mag-ang 'rect (lambda (r a) (tag (make-from-mag-ang r a))))
  (put 'real-part '(rect) real-part)
  (put 'imag-part '(rect) imag-part)
  (put 'magnitude '(rect) magnitude)
  (put 'angle '(rect) angle)
  'done)

(install-rectangular-complex-number-package)