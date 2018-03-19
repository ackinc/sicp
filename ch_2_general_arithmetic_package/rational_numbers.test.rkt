#lang racket
(require "scheme_numbers.package.rkt") ; for =zero? and project on ordinary numbers
(require "rational_numbers.package.rkt")
(require "general_arithmetic_ops.rkt")
(require (only-in "../ch_2_list_operations.rkt"
                  list-equal?))
(require "complex_numbers.package.rkt") ; for raise

(display "make")(newline)
(list-equal? (make-rational 3 4) (list 'rational 3 4))

(display "numer and denom")(newline)
(= (numer (make-rational 3 4)) 3)
(= (denom (make-rational 3 4)) 4)

(display "arithmetic ops")(newline)
(list-equal? (add (make-rational 3 4) (make-rational 5 4)) (make-rational 2 1)) ; unless apply-generic drops result to lowest type in tower
(list-equal? (sub (make-rational 3 4) (make-rational 5 4)) (make-rational -1 2)) ; unless apply-generic drops result to lowest type in tower
(list-equal? (mul (make-rational 0 4) (make-rational 5 4)) (make-rational 0 1))
(list-equal? (mul (make-rational 3 4) (make-rational 5 4)) (make-rational 15 16))
(list-equal? (div (make-rational 3 4) (make-rational 5 4)) (make-rational 3 5))

(display "equ?")(newline)
(eq? (equ? (make-rational 3 4) (make-rational 6 8)) #t)
(eq? (equ? (make-rational 3 4) (make-rational 3 5)) #f)

(display "=zero?")(newline)
(eq? (=zero? (make-rational 0 3)) #t)
(eq? (=zero? (make-rational 1 3)) #f)

(display "negate")(newline)
(list-equal? (negate (make-rational 0 3)) (make-rational 0 3))
(list-equal? (negate (make-rational 1 3)) (make-rational -1 3))

(display "raise")(newline)
(list-equal? (raise (make-rational 0 1)) '(complex rect 0 0))
(list-equal? (raise (make-rational 9 1)) '(complex rect 9 0))
(list-equal? (raise (make-rational 9.0 2)) '(complex rect 4.5 0))

(display "project")(newline)
(= (project (make-rational 9 1)) 9)
(= (project (make-rational 9 2)) 4)