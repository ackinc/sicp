#lang racket
(require "./ch_2_type_tag_helpers.rkt")

(provide install-rational-number-package)

(define (install-rational-number-package)
  ;internal procedures
  (define (make n d)
    (let ((n-abs (abs n))
          (d-abs (abs d))
          (pos-rat? (or (and (>= n 0) (> d 0))
                        (and (< n 0) (< d 0))))
          (g (gcd (abs n) (abs d))))
      (cons ((if pos-rat? + -) (/ n-abs g)) (/ d-abs g))))

  (define (numer rat) (car rat))
  (define (denom rat) (cdr rat))

  (define (add r1 r2)
    (let ((n1 (numer r1))
          (d1 (denom r1))
          (n2 (numer r2))
          (d2 (denom r2)))
      (make (+ (* n1 d2) (* n2 d1)) (* d1 d2))))

  (define (sub r1 r2)
    (let ((n1 (numer r1))
          (d1 (denom r1))
          (n2 (numer r2))
          (d2 (denom r2)))
      (make (- (* n1 d2) (* n2 d1)) (* d1 d2))))

  (define (mul r1 r2)
    (let ((n1 (numer r1))
          (d1 (denom r1))
          (n2 (numer r2))
          (d2 (denom r2)))
      (make (* n1 n2) (* d1 d2))))

  (define (div r1 r2)
    (let ((n1 (numer r1))
          (d1 (denom r1))
          (n2 (numer r2))
          (d2 (denom r2)))
      (make (* n1 d2) (* n2 d1))))

  ;public interface
  (define (tag datum) (attach-tag 'rational datum))
  (put 'add '(rational rational) (lambda (r1 r2) (tag (add r1 r2))))
  (put 'sub '(rational rational) (lambda (r1 r2) (tag (sub r1 r2))))
  (put 'mul '(rational rational) (lambda (r1 r2) (tag (mul r1 r2))))
  (put 'div '(rational rational) (lambda (r1 r2) (tag (div r1 r2))))
  (put 'make 'rational (lambda (n d) (tag (make n d))))
  'done)