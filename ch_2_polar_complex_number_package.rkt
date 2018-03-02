#lang racket
(require "./ch_2_type_tag_helpers.rkt")

(provide install-polar-complex-number-package)

(define (square x) (* x x))
(define (install-polar-complex-number-package)
  (define (magnitude z) (car z))
  (define (angle z) (cdr z))
  (define (real-part z) (* (magnitude z) (cos (angle z))))
  (define (imag-part z) (* (magnitude z) (sin (angle z))))

  (define (make-from-mag-ang r a) (cons r a))
  (define (make-from-real-imag x y) (cons (sqrt (+ (square x) (square y))) (atan y x)))

  (define (tag datum) (attach-tag 'polar datum))
  (put 'make-from-mag-ang 'polar (lambda (r a) (tag (make-from-mag-ang r a))))
  (put 'make-from-real-imag 'polar (lambda (x y) (tag (make-from-real-imag x y))))
  (put 'real-part ('polar) real-part)
  (put 'imag-part ('polar) imag-part)
  (put 'magnitude ('polar) magnitude)
  (put 'angle ('polar) angle)
  'done)