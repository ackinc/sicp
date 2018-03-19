#lang racket
(require "scheme_numbers.package.rkt")
(require "rational_numbers.package.rkt") ; for raise
(require "general_arithmetic_ops.rkt")
(require (only-in "../ch_2_list_operations.rkt"
                  list-equal?))

(display "make")(newline)
(= (make-scheme-number 9) 9)

(display "arithmetic ops")(newline)
(= (add (make-scheme-number 9) (make-scheme-number 3)) 12)
(= (sub (make-scheme-number 9) (make-scheme-number 3)) 6)
(= (mul (make-scheme-number 9) (make-scheme-number 3)) 27)
(= (div (make-scheme-number 9) (make-scheme-number 3)) 3)

(display "equ?")(newline)
(eq? (equ? (make-scheme-number 9) (make-scheme-number 9)) #t)
(eq? (equ? (make-scheme-number 9) (make-scheme-number 8)) #f)

(display "=zero?")(newline)
(eq? (=zero? (make-scheme-number 0)) #t)
(eq? (=zero? (make-scheme-number 3)) #f)

(display "raise")(newline)
(list-equal? (raise (make-scheme-number 9)) '(rational 9 1))

(display "negate")(newline)
(= (negate 0) 0)
(= (negate 1) -1)