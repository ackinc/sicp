#lang racket
(require "ch_2_type_tag_helpers.rkt")
(require "ch_2_hash_ops.rkt")
(require "ch_2_apply_generic_op.rkt")
(require "ch_2_rect_complex_number_package.rkt")
(require "ch_2_polar_complex_number_package.rkt")

(provide make-complex-from-real-imag
         make-complex-from-mag-ang)

(define (install-complex-number-package)
  (define (make-from-real-imag x y)
    ((get 'make-from-real-imag 'rect) x y))

  (define (make-from-mag-ang r a)
    ((get 'make-from-mag-ang 'polar) r a))

  (define (real-part z) (apply-generic-op 'real-part z))
  (define (imag-part z) (apply-generic-op 'imag-part z))
  (define (magnitude z) (apply-generic-op 'magnitude z))
  (define (angle z) (apply-generic-op 'angle z))

  (define (add z1 z2)
    (make-from-real-imag (+ (real-part z1) (real-part z2))
                         (+ (imag-part z1) (imag-part z2))))

  (define (sub z1 z2)
    (make-from-real-imag (- (real-part z1) (real-part z2))
                         (- (imag-part z1) (imag-part z2))))

  (define (mul z1 z2)
    (make-from-mag-ang (* (magnitude z1) (magnitude z2))
                       (+ (angle z1) (angle z2))))

  (define (div z1 z2)
    (make-from-mag-ang (/ (magnitude z1) (magnitude z2))
                       (- (angle z1) (angle z2))))

  (define (equ? z1 z2)
    (and (= (real-part z1) (real-part z2))
         (= (imag-part z1) (imag-part z2))))

  (define (=zero? z)
    (and (= (real-part z) 0) (= (imag-part z) 0)))

  (define (tag datum) (attach-tag 'complex datum))
  (put 'make-from-real-imag 'complex (lambda (x y) (tag (make-from-real-imag x y))))
  (put 'make-from-mag-ang 'complex (lambda (r a) (tag (make-from-mag-ang r a))))
  
  (put 'real-part 'complex real-part)
  (put 'imag-part 'complex imag-part)
  (put 'magnitude 'complex magnitude)
  (put 'angle 'complex angle)

  (put 'add '(complex complex) (lambda (z1 z2) (tag (add z1 z2))))
  (put 'sub '(complex complex) (lambda (z1 z2) (tag (sub z1 z2))))
  (put 'mul '(complex complex) (lambda (z1 z2) (tag (mul z1 z2))))
  (put 'div '(complex complex) (lambda (z1 z2) (tag (div z1 z2))))
  (put 'equ? '(complex complex) equ?)
  (put '=zero? 'complex =zero?)

  ; ex 2.83
  (put-coercion '(rational complex)
                (lambda (rat)
                  (tag (make-from-real-imag (/ ((get 'numer 'rational) rat) ((get 'denom 'rational) rat)) 0))))

  ; ex 2.85
  (put 'project 'complex (get-coercion '(complex rational)))
  
  'done)

(install-complex-number-package)

(define (make-complex-from-real-imag x y)
  ((get 'make-from-real-imag 'complex) x y))

(define (make-complex-from-mag-ang r a)
  ((get 'make-from-mag-ang 'complex) r a))