#lang racket
(define (deriv exp v)
  (cond ((number? exp) 0)
        ((variable? exp) (if (same-variable? exp v) 1 0))
        ((sum? exp) (make-sum (deriv (addend exp) v)
                              (deriv (augend exp) v)))
        ((product? exp) (make-sum (make-product (multiplicand exp)
                                                (deriv (multiplier exp) v))
                                  (make-product (multiplier exp)
                                                (deriv (multiplicand exp) v))))
        (else (error "deriv: invalid expression type" exp))))

(define (variable? v) (symbol? v))
(define (same-variable? v1 v2) (and (variable? v1) (variable? v2) (eq? v1 v2)))

(define (=number? exp num)
  (and (number? exp) (= exp num)))

; if elem is in items, return sublist starting at elem
; else return #f
(define (memq elem items)
  (cond ((null? items) #f)
        ((eq? (car items) elem) items)
        (else (memq elem (cdr items)))))

; if elem is in items, return sublist [0, index-of-elem)
; else return items
(define (memq-inverse elem items)
  (cond ((or (null? items) (eq? (car items) elem)) null)
        (else (cons (car items) (memq-inverse elem (cdr items))))))

(define (make-sum exp1 exp2)
  (cond ((=number? exp1 0) exp2)
        ((=number? exp2 0) exp1)
        ((and (number? exp1) (number? exp2)) (+ exp1 exp2))
        (else (list exp1 '+ exp2))))

;(define (sum? exp) (and (list? exp) (eq? (cadr exp) '+))) ; a
(define (sum? exp) (and (list? exp) (not (eq? (memq '+ exp) #f)))) ; b

;(define (addend sum-exp) (car sum-exp)) ; a
; b
(define (addend sum-exp)
  (let ((exp (memq-inverse '+ sum-exp)))
    (if (null? (cdr exp)) (car exp) exp)))

;(define (augend sum-exp) (caddr sum-exp)) ; a
; b
(define (augend sum-exp)
  (let ((exp (cdr (memq '+ sum-exp))))
    (if (null? (cdr exp)) (car exp) exp)))

(define (make-product exp1 exp2)
  (cond ((or (=number? exp1 0) (=number? exp2 0)) 0)
        ((=number? exp1 1) exp2)
        ((=number? exp2 1) exp1)
        ((and (number? exp1) (number? exp2)) (* exp1 exp2))
        (else (list exp1 '* exp2))))
(define (product? exp) (and (list? exp) (eq? (cadr exp) '*)))
(define (multiplier prod-exp) (car prod-exp))
;(define (multiplicand prod-exp) (caddr prod-exp)) ; a

; b
(define (multiplicand prod-exp)
  (if (null? (cdddr prod-exp))
      (caddr prod-exp)
      (cddr prod-exp)))
