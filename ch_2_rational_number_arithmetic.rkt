#lang racket
(require "./ch_1_gcd.rkt")

; ex 2.1
(define (make-rat n d)
  (let ((n-abs (abs n))
        (d-abs (abs d))
        (pos-rat? (or (and (>= n 0) (> d 0))
                      (and (< n 0) (< d 0))))
        (g (gcd (abs n) (abs d))))
    (cons ((if pos-rat? + -) (/ n-abs g)) (/ d-abs g))))

(define (numer rat) (car rat))
(define (denom rat) (cdr rat))

(define (add-rat r1 r2)
  (let ((n1 (numer r1))
        (d1 (denom r1))
        (n2 (numer r2))
        (d2 (denom r2)))
    (make-rat (+ (* n1 d2) (* n2 d1)) (* d1 d2))))

(define (sub-rat r1 r2)
  (let ((n1 (numer r1))
        (d1 (denom r1))
        (n2 (numer r2))
        (d2 (denom r2)))
    (make-rat (- (* n1 d2) (* n2 d1)) (* d1 d2))))

(define (mul-rat r1 r2)
  (let ((n1 (numer r1))
        (d1 (denom r1))
        (n2 (numer r2))
        (d2 (denom r2)))
    (make-rat (* n1 n2) (* d1 d2))))

(define (div-rat r1 r2)
  (let ((n1 (numer r1))
        (d1 (denom r1))
        (n2 (numer r2))
        (d2 (denom r2)))
    (make-rat (* n1 d2) (* n2 d1))))

(define (eq-rat r1 r2)
  (let ((n1 (numer r1))
        (d1 (denom r1))
        (n2 (numer r2))
        (d2 (denom r2)))
    (= (* n1 d2) (* n2 d1))))

(define (print-rat r)
  (display (numer r))
  (display "/")
  (display (denom r)))