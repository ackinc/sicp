#lang racket
(require "hash_ops.rkt")
(require "type_tag_helpers.rkt")

(provide make-scheme-number)

(define (install-scheme-number-package)
  (define (tag datum) (attach-tag 'scheme-number datum))
  (put 'make 'scheme-number (lambda (x) (tag x)))

  (put 'add '(scheme-number scheme-number) (lambda (x y) (tag (+ x y))))
  (put 'sub '(scheme-number scheme-number) (lambda (x y) (tag (- x y))))
  (put 'mul '(scheme-number scheme-number) (lambda (x y) (tag (* x y))))
  (put 'div '(scheme-number scheme-number) (lambda (x y) (tag (/ x y))))
  (put 'equ? '(scheme-number scheme-number) =)
  (put '=zero? '(scheme-number) (lambda (x) (= x 0)))

  ; ex 2.83
  (put 'raise '(scheme-number) (lambda (n) ((get-coercion '(scheme-number rational)) n)))

  ; ex 2.85
  (put-coercion '(rational scheme-number) (lambda (rat) (round (/ ((get 'numer 'rational) rat) ((get 'denom 'rational) rat)))))

  ; ex 2.86
  (put 'cos '(scheme-number) cos)
  (put 'sin '(scheme-number) sin)
  (put 'atan '(scheme-number scheme-number) atan)
  (put 'sqrt '(scheme-number) sqrt)

  ; ex 2.88
  (put 'negate '(scheme-number) (lambda (x) (* x -1)))

  'installed-scheme-numbers-package)

(install-scheme-number-package)
(define (make-scheme-number n) ((get 'make 'scheme-number) n))
