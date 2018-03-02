#lang racket
(require "./ch_2_type_tag_helpers.rkt")

(provide install-scheme-number-package)

(define (install-scheme-number-package)
  (define (tag datum) (attach-tag 'scheme-number datum))
  (put 'add '(scheme-number scheme-number) (lambda (x y) (tag (+ x y))))
  (put 'sub '(scheme-number scheme-number) (lambda (x y) (tag (- x y))))
  (put 'mul '(scheme-number scheme-number) (lambda (x y) (tag (* x y))))
  (put 'div '(scheme-number scheme-number) (lambda (x y) (tag (/ x y))))
  (put 'make 'scheme-number (lambda (x) (tag x)))
  'done)