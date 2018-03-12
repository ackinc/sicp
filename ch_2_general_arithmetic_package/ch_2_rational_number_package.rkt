#lang racket
(require "ch_2_type_tag_helpers.rkt")
(require "ch_2_hash_ops.rkt")

(provide make-rational)

(define (install-rational-number-package)
  ;internal procedures
  (define (make n d)
    (if (= d 0)
        (error "Denom cannot be 0" n d)
        (let ((n-abs (abs n))
              (d-abs (abs d))
              (pos-rat? (or (and (>= n 0) (> d 0))
                            (and (< n 0) (< d 0))))
              (g (gcd (abs n) (abs d))))
          (cons ((if pos-rat? + -) (/ n-abs g)) (/ d-abs g)))))

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

  (define (equ? r1 r2)
    (let ((n1 (numer r1))
          (d1 (denom r1))
          (n2 (numer r2))
          (d2 (denom r2)))
      (= (* n1 d2) (* n2 d1))))

  (define (=zero? rat)
    (= (numer rat) 0))

  ;public interface
  (define (tag datum) (attach-tag 'rational datum))
  (put 'make 'rational (lambda (n d) (tag (make n d))))

  (put 'numer 'rational numer)
  (put 'denom 'rational denom)

  (put 'add '(rational rational) (lambda (r1 r2) (tag (add r1 r2))))
  (put 'sub '(rational rational) (lambda (r1 r2) (tag (sub r1 r2))))
  (put 'mul '(rational rational) (lambda (r1 r2) (tag (mul r1 r2))))
  (put 'div '(rational rational) (lambda (r1 r2) (tag (div r1 r2))))
  (put 'equ? '(rational rational) equ?)
  (put '=zero? 'rational =zero?)

  ; ex 2.83
  (put-coercion '(scheme-number rational) (lambda (n) (tag (make n 1))))
  (put 'raise 'rational (lambda (rat) ((get-coercion '(rational complex)) rat)))

  ; ex 2.85
  (put-coercion '(complex rational) (lambda (n) (tag (make ((get 'real-part 'complex) n) 1))))
  (put 'project 'rational (get-coercion '(rational scheme-number)))

  ; ex 2.86
  (put 'sin 'rational (lambda (rat) (sin (/ (numer rat) (denom rat)))))
  (put 'cos 'rational (lambda (rat) (cos (/ (numer rat) (denom rat)))))
  (put 'atan '(rational rational) (lambda (r1 r2)
                                    (atan (/ (numer r1) (denom r1)) (/ (numer r2) (denom r2)))))
  (put 'sqrt 'rational (lambda (rat) (make (sqrt (numer rat)) (sqrt (denom rat)))))

  'done)

(install-rational-number-package)

(define (make-rational n d) ((get 'make 'rational) n d))
