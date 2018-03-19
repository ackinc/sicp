#lang racket
(require "apply_generic.rkt")
(require "complex_numbers__polar.package.rkt")
(require "complex_numbers__rectangular.package.rkt")
(require "general_arithmetic_ops.rkt")
(require "hash_ops.rkt")
(require "type_tag_helpers.rkt")

(provide make-complex-from-real-imag
         make-complex-from-mag-ang)

(define (install-complex-number-package)
  (define (make-from-real-imag x y)
    ((get 'make-from-real-imag 'rect) x y))

  (define (make-from-mag-ang r a)
    ((get 'make-from-mag-ang 'polar) r a))

  (define (real-part z) (apply-generic 'real-part z))
  (define (imag-part z) (apply-generic 'imag-part z))
  (define (magnitude z) (apply-generic 'magnitude z))
  (define (angle z) (apply-generic 'angle z))

  (define (add_c z1 z2)
    (make-from-real-imag (add (real-part z1) (real-part z2))
                         (add (imag-part z1) (imag-part z2))))

  (define (sub_c z1 z2)
    (make-from-real-imag (sub (real-part z1) (real-part z2))
                         (sub (imag-part z1) (imag-part z2))))

  (define (mul_c z1 z2)
    (make-from-mag-ang (mul (magnitude z1) (magnitude z2))
                       (add (angle z1) (angle z2))))

  (define (div_c z1 z2)
    (make-from-mag-ang (div (magnitude z1) (magnitude z2))
                       (sub (angle z1) (angle z2))))

  (define (equ?_c z1 z2)
    (and (equ? (real-part z1) (real-part z2))
         (equ? (imag-part z1) (imag-part z2))))

  (define (=zero?_c z)
    (and (equ? (real-part z) 0) (equ? (imag-part z) 0)))

  (define (tag datum) (attach-tag 'complex datum))
  (put 'make-from-real-imag 'complex (lambda (x y) (tag (make-from-real-imag x y))))
  (put 'make-from-mag-ang 'complex (lambda (r a) (tag (make-from-mag-ang r a))))
  
  (put 'real-part 'complex real-part)
  (put 'imag-part 'complex imag-part)
  (put 'magnitude 'complex magnitude)
  (put 'angle 'complex angle)

  (put 'add '(complex complex) (lambda (z1 z2) (tag (add_c z1 z2))))
  (put 'sub '(complex complex) (lambda (z1 z2) (tag (sub_c z1 z2))))
  (put 'mul '(complex complex) (lambda (z1 z2) (tag (mul_c z1 z2))))
  (put 'div '(complex complex) (lambda (z1 z2) (tag (div_c z1 z2))))
  (put 'equ? '(complex complex) equ?_c)
  (put '=zero? 'complex =zero?_c)

  ; ex 2.83
  (put-coercion '(rational complex)
                (lambda (rat)
                  (tag (make-from-real-imag (/ ((get 'numer 'rational) rat) ((get 'denom 'rational) rat)) 0))))

  ; ex 2.85
  (put 'project 'complex (get-coercion '(complex rational)))

  ; ex 2.88
  (put 'negate 'complex (lambda (z) (tag (negate z))))
  
  'installed-complex-number-package)

(install-complex-number-package)

(define (make-complex-from-real-imag x y)
  ((get 'make-from-real-imag 'complex) x y))

(define (make-complex-from-mag-ang r a)
  ((get 'make-from-mag-ang 'complex) r a))
