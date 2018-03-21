#lang racket
(require "apply_generic.rkt")
(require "complex_numbers__rectangular.package.rkt")
(require "general_arithmetic_ops.rkt")
(require "hash_ops.rkt")
(require "scheme_numbers.package.rkt") ; need generic ops on ordinary numbers to be part of the dispatch table
(require (only-in "../ch_2_list_operations.rkt"
                  list-equal?))

(define (make x y) ((get 'make-from-real-imag 'rect) x y))
(define (make-alt r a) ((get 'make-from-mag-ang 'rect) r a))
(define (real-part z) (apply-generic 'real-part z))
(define (imag-part z) (apply-generic 'imag-part z))
(define (magnitude z) (apply-generic 'magnitude z))
(define (angle z) (apply-generic 'angle z))

; TESTS
(display "constructor")(newline)
(list-equal? (make 5 0) '(rect 5 0))
(list-equal? (make-alt 5 (/ pi 2)) '(rect 0 5)) ; technically #t, but will return #f due to lack of precision

(display "selectors")(newline)
(= (real-part (make 5 0)) 5)
(= (imag-part (make 5 0)) 0)
(= (magnitude (make 5 0)) 5)
(= (magnitude (make 3 4)) 5)
(= (angle (make 5 0)) 0)
(= (angle (make 1 1)) (/ pi 4))

(display "negate")(newline)
(list-equal? (negate (make 3 4)) (make -3 -4))