#lang racket
(require "polynomials.package.rkt")
(require "general_arithmetic_ops.rkt")
(require (only-in "../ch_2_list_operations.rkt"
                  list-equal?))

(display "make-poly")(newline)
(list-equal? (make-polynomial 'x '()) '(polynomial x dense))
(list-equal? (make-polynomial 'x '(3 2 1)) '(polynomial x dense 3 2 1))
(list-equal? (make-polynomial 'x '(1 0 0 0 0 1)) '(polynomial x sparse (5 1) (0 1)))
(list-equal? (make-polynomial 'x '((100 3) (3 8) (0 -1))) '(polynomial x sparse (100 3) (3 8) (0 -1)))
(list-equal? (make-polynomial 'x '((2 3) (1 2) (0 1))) '(polynomial x dense 3 2 1))

(display "variable and term-list selectors")(newline)
(eq? (variable '(polynomial x dense 3 2 1)) 'x)
(list-equal? (term-list '(polynomial x dense 3 2 1)) '(dense 3 2 1))

(display "polynomial arithmetic")(newline)
(list-equal? (add (make-polynomial 'x '(3 2 1)) (make-polynomial 'x '(4 3 2 1))) '(polynomial x dense 4 6 4 2))
(list-equal? (sub (make-polynomial 'x '(3 2 1)) (make-polynomial 'x '(4 3 2 1))) '(polynomial x sparse (3 -4)))
(list-equal? (mul (make-polynomial 'x '(3 2 1)) (make-polynomial 'x '())) '(polynomial x dense))
(list-equal? (mul (make-polynomial 'x '(3 2 1)) (make-polynomial 'x '(2 0))) '(polynomial x dense 6 4 2 0))
(list-equal? (div (make-polynomial 'x '(1 0 0 0 0 -1)) (make-polynomial 'x '(1 0 -1)))
             (list (make-polynomial 'x '(1 0 1 0))
                   (make-polynomial 'x '(1 -1))))

(display "equ?")(newline)
(eq? (equ? (make-polynomial 'x '(3 2 1)) (make-polynomial 'x '(3 2 1))) #t)
(eq? (equ? (make-polynomial 'x '(3 2 1)) (make-polynomial 'y '(3 2 1))) #f) ; diff variable
(eq? (equ? (make-polynomial 'x '(3 2 1)) (make-polynomial 'x '(4 2 1))) #f) ; diff coeffs
(eq? (equ? (make-polynomial 'x '(3 2 1)) (make-polynomial 'x '((2 3) (1 2) (0 1)))) #t)

(display "negate")(newline)
(list-equal? (negate (make-polynomial 'x '())) '(polynomial x dense))
(list-equal? (negate (make-polynomial 'x '((100 3) (25 2) (1 1)))) '(polynomial x sparse (100 -3) (25 -2) (1 -1)))
(list-equal? (negate (make-polynomial 'x '(3 2 1))) '(polynomial x dense -3 -2 -1))

(display "=zero?")(newline)
(=zero? (make-polynomial 'x '()))
(=zero? (add (make-polynomial 'x '(3 2 1)) (make-polynomial 'x '(-3 -2 -1))))