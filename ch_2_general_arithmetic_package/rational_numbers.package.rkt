#lang racket
(require "type_tag_helpers.rkt")
(require "hash_ops.rkt")
(require "general_arithmetic_ops.rkt")

(provide make-rational numer denom)

(define (install-rational-number-package)
  ;internal procedures

  ; pre ex 2.93
  ;(define (make n d)
  ;  (if (= d 0)
  ;      (error "Denom cannot be 0" n d)
  ;      (let ((n-abs (abs n))
  ;            (d-abs (abs d))
  ;            (pos-rat? (or (and (>= n 0) (> d 0))
  ;                          (and (< n 0) (< d 0))))
  ;            (g (gcd (abs n) (abs d))))
  ;        (list ((if pos-rat? + -) (/ n-abs g)) (/ d-abs g)))))

  ; for ex 2.93 - not reducing rational to lowest terms
  ;(define (make n d)
  ;  (if (=zero? d)
  ;      (error "Denom cannot be zero" n d)
  ;        (list n d)))
  
  ; ex 2.97 - reducing rational polynomials to lowest terms
  (define (make n d)
    (if (=zero? d)
        (error "Denom cannot be zero" n d)
        (let ((result (reduce n d)))
          (list (car result) (cadr result)))))

  (define (numer rat) (car rat))
  (define (denom rat) (cadr rat))

  (define (add-rat r1 r2)
    (let ((n1 (numer r1))
          (d1 (denom r1))
          (n2 (numer r2))
          (d2 (denom r2)))
      (make (add (mul n1 d2) (mul n2 d1)) (mul d1 d2))))

  (define (sub-rat r1 r2)
    (let ((n1 (numer r1))
          (d1 (denom r1))
          (n2 (numer r2))
          (d2 (denom r2)))
      (make (sub (mul n1 d2) (mul n2 d1)) (mul d1 d2))))

  (define (mul-rat r1 r2)
    (let ((n1 (numer r1))
          (d1 (denom r1))
          (n2 (numer r2))
          (d2 (denom r2)))
      (make (mul n1 n2) (mul d1 d2))))

  (define (div-rat r1 r2)
    (let ((n1 (numer r1))
          (d1 (denom r1))
          (n2 (numer r2))
          (d2 (denom r2)))
      (make (mul n1 d2) (mul n2 d1))))

  (define (equ?-rat r1 r2)
    (let ((n1 (numer r1))
          (d1 (denom r1))
          (n2 (numer r2))
          (d2 (denom r2)))
      (equ? (mul n1 d2) (mul n2 d1))))

  (define (=zero?-rat rat)
    (=zero? (numer rat)))

  ;public interface
  (define (tag datum) (attach-tag 'rational datum))
  (put 'make 'rational (lambda (n d) (tag (make n d))))

  (put 'numer 'rational numer)
  (put 'denom 'rational denom)

  (put 'add '(rational rational) (lambda (r1 r2) (tag (add-rat r1 r2))))
  (put 'sub '(rational rational) (lambda (r1 r2) (tag (sub-rat r1 r2))))
  (put 'mul '(rational rational) (lambda (r1 r2) (tag (mul-rat r1 r2))))
  (put 'div '(rational rational) (lambda (r1 r2) (tag (div-rat r1 r2))))
  (put 'equ? '(rational rational) equ?-rat)
  (put '=zero? '(rational) =zero?-rat)

  ; ex 2.83
  (put-coercion '(scheme-number rational) (lambda (n) (tag (make n 1))))
  (put 'raise '(rational) (lambda (rat) ((get-coercion '(rational complex)) rat)))

  ; ex 2.85
  (put-coercion '(complex rational) (lambda (n) (tag (make ((get 'real-part '(complex)) n) 1))))
  (put 'project '(rational) (get-coercion '(rational scheme-number)))

  ; ex 2.86
  (put 'sin 'rational (lambda (rat) (sin (/ (numer rat) (denom rat)))))
  (put 'cos 'rational (lambda (rat) (cos (/ (numer rat) (denom rat)))))
  (put 'atan '(rational rational) (lambda (r1 r2)
                                    (atan (/ (numer r1) (denom r1)) (/ (numer r2) (denom r2)))))
  (put 'sqrt 'rational (lambda (rat) (tag (make (sqrt (numer rat)) (sqrt (denom rat))))))

  ; ex 2.88
  (put 'negate '(rational) (lambda (rat) (tag (make (* (numer rat) -1) (denom rat)))))

  'installed-rational-numbers-package)

(install-rational-number-package)

(define (make-rational n d) ((get 'make 'rational) n d))
(define (numer rat) ((get 'numer 'rational) (contents rat)))
(define (denom rat) ((get 'denom 'rational) (contents rat)))