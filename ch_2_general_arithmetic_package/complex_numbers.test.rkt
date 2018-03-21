#lang racket
(require "complex_numbers.package.rkt")
(require "general_arithmetic_ops.rkt")
(require "scheme_numbers.package.rkt")
(require "rational_numbers.package.rkt") ; for complex -> rational conversion in project
(require (only-in "../ch_2_list_operations.rkt"
                  list-equal?))

(display "constructors")(newline)
(define z (make-complex-from-real-imag 3 4))
(define z1 (make-complex-from-mag-ang 5 1))
(list-equal? z '(complex rect 3 4))
(list-equal? (make-complex-from-mag-ang 5 1) '(complex polar 5 1))

(display "selectors")(newline)
(= (real-part z) 3)
(= (imag-part z) 4)
(= (magnitude z) 5)
(= (angle z) (atan 4 3))

(display "arithmetic ops")(newline)
(list-equal? (add z z) (make-complex-from-real-imag 6 8))
(list-equal? (sub z z) (make-complex-from-real-imag 0 0))
(list-equal? (mul z z) (make-complex-from-mag-ang 25 (+ (atan 4 3) (atan 4 3))))
(list-equal? (div z z) (make-complex-from-mag-ang 1 0.0)) ; weirdly returns #f if using 0 instead of 0.0

(display "equ?")(newline)
(eq? (equ? z z) #t)
(eq? (equ? z z1) #f)

(display "=zero?")(newline)
(eq? (=zero? (make-complex-from-real-imag 0 0)) #t)
(eq? (=zero? (make-complex-from-mag-ang 0 0)) #t)
(eq? (=zero? (make-complex-from-real-imag 1 0)) #f)
(eq? (=zero? (make-complex-from-mag-ang 1 0)) #f)

(display "negate")(newline)
(list-equal? (negate z) '(complex rect -3 -4))

(display "project")(newline)
(list-equal? (project z) '(rational 3 1))