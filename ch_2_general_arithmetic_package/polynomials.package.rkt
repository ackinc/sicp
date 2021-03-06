#lang racket
(require "type_tag_helpers.rkt")
(require "hash_ops.rkt")
(require "general_arithmetic_ops.rkt")
(require "apply_generic.rkt")
(require "polynomials__term_ops.rkt")
(require "polynomials__dense_termlists.package.rkt")
(require "polynomials__sparse_termlists.package.rkt")
(require (only-in "../ch_2_list_operations.rkt"
                  list-equal?))

(provide make-polynomial variable term-list)

(define (install-polynomial-package)
  (define (make-poly variable terms)
    (let ((termlist (cond ((or (null? terms) (number? (car terms))) ((get 'make 'dense) terms))
                          ((symbol? (car terms)) terms)
                          (else ((get 'make 'sparse) terms)))))
      (cons variable (optimize termlist))))

  (define (optimize termlist) ; converts a 'dense' termlist to a sparse one if it is actually sparse, and vice versa
    (if (is-appropriate-type termlist)
        termlist
        (convert-to-other-type termlist)))
    ;termlist)

  (define (variable poly) (car poly))
  (define (term-list poly) (cdr poly))

  (define (same-variable? v1 v2) (eq? v1 v2))

  (define (the-empty-termlist) ((get 'the-empty-termlist 'dense)))
  (define (empty-termlist? tl) (apply-generic 'empty-termlist? tl))
  (define (first-term tl) (apply-generic 'first-term tl))
  (define (rest-terms tl) (apply-generic 'rest-terms tl))
  (define (adjoin-term t tl) ((get 'adjoin-term (type-tag tl)) t (contents tl)))
  (define (is-appropriate-type tl) (apply-generic 'is-appropriate-type tl))
  (define (convert-to-other-type tl) (apply-generic 'convert-to-other-type tl))

  ; polynomial addition
  (define (add-terms tl1 tl2)
    (cond ((empty-termlist? tl1) tl2)
          ((empty-termlist? tl2) tl1)
          (else (let* ((t1 (first-term tl1))
                       (t2 (first-term tl2))
                       (o1 (order t1))
                       (o2 (order t2)))
                  (cond ((> o1 o2) (adjoin-term t1 (add-terms (rest-terms tl1) tl2)))
                        ((< o1 o2) (adjoin-term t2 (add-terms tl1 (rest-terms tl2))))
                        (else (adjoin-term (make-term o1 (add (coeff t1) (coeff t2)))
                                           (add-terms (rest-terms tl1) (rest-terms tl2)))))))))
  (define (add-poly p1 p2)
    (let ((v1 (variable p1))
          (v2 (variable p2)))
      (if (same-variable? v1 v2)
          (make-poly v1 (add-terms (term-list p1)
                                   (term-list p2)))
          (error "ADD-POLY -- polynomials must have the same indeterminate" (list p1 p2)))))

  ; polynomial subtraction
  (define (negate-terms tl)
    (if (empty-termlist? tl)
        (the-empty-termlist)
        (let ((ft (first-term tl)))
          (adjoin-term (make-term (order ft) (negate (coeff ft)))
                       (negate-terms (rest-terms tl))))))
  (define (negate-poly p)
    (make-poly (variable p) (negate-terms (term-list p))))
  (define (sub-poly p1 p2)
    (add-poly p1 (negate-poly p2)))

  ; polynomial multiplication
  (define (mul-term-by-all-terms t tl)
    (if (empty-termlist? tl)
        (the-empty-termlist)
        (let ((ft (first-term tl)))
          (adjoin-term (make-term (add (order t) (order ft)) (mul (coeff t) (coeff ft)))
                       (mul-term-by-all-terms t (rest-terms tl))))))
  (define (mul-terms tl1 tl2)
    (cond ((or (empty-termlist? tl1) (empty-termlist? tl2)) (the-empty-termlist))
          (else (add-terms (mul-term-by-all-terms (first-term tl1) tl2)
                           (mul-terms (rest-terms tl1) tl2)))))
  (define (mul-poly p1 p2)
    (let ((v1 (variable p1))
          (v2 (variable p2)))
      (if (same-variable? v1 v2)
          (make-poly v1 (mul-terms (term-list p1)
                                   (term-list p2)))
          (error "MUL-POLY -- polynomials must have the same indeterminate" (list p1 p2)))))

  ; polynomial division
  (define (div-terms L1 L2)
    (if (empty-termlist? L1)
        (list (the-empty-termlist) (the-empty-termlist))
        (let ((ft1 (first-term L1))
              (ft2 (first-term L2)))
          (if (> (order ft2) (order ft1))
              (list (the-empty-termlist) L1)
              (let* ((new-c (div (coeff ft1) (coeff ft2)))
                     (new-o (sub (order ft1) (order ft2)))
                     (new-term (make-term new-o new-c))
                     (rest-of-result (div-terms
                                      (add-terms
                                       L1
                                       (negate-terms
                                        (mul-term-by-all-terms new-term L2)))
                                      L2)))
                  (list (adjoin-term new-term (car rest-of-result)) (cadr rest-of-result)))))))
  (define (quotient-terms tl1 tl2) (car (div-terms tl1 tl2)))
  (define (remainder-terms tl1 tl2) (cadr (div-terms tl1 tl2)))
  (define (div-poly p1 p2)
    (let ((v1 (variable p1))
          (v2 (variable p2)))
      (if (same-variable? v1 v2)
          (let* ((div-result (div-terms (term-list p1) (term-list p2)))
                 (q (car div-result))
                 (r (cadr div-result)))
            (list (make-poly v1 q) (make-poly v1 r)))
          (error "DIV-POLY -- polynomials must have the same indeterminate" (list p1 p2)))))

  ; equ?
  (define (equ-terms? tl1 tl2)
    (cond ((and (empty-termlist? tl1) (empty-termlist? tl2)) #t)
          ((or (empty-termlist? tl1) (empty-termlist? tl2)) #f)
          (else (let ((ft1 (first-term tl1))
                      (ft2 (first-term tl2)))
                  (and (= (order ft1) (order ft2))
                       (equ? (coeff ft1) (coeff ft2))
                       (equ-terms? (rest-terms tl1) (rest-terms tl2)))))))
  (define (equ?-poly p1 p2) (and (same-variable? (variable p1) (variable p2))
                                 (equ-terms? (term-list p1) (term-list p2))))

  ; =zero?
  (define (=zero?-poly p) (empty-termlist? (term-list p)))

  ; gcd
  ; ex 2.96a
  (define (pseudoremainder-terms tl1 tl2)
    (let* ((o1 (order (first-term tl1)))
           (ft2 (first-term tl2))
           (o2 (order ft2))
           (c2 (coeff ft2))
           (integerizing-factor (expt c2 (+ 1 (- o1 o2))))
           (ift (make-term 0 integerizing-factor)))
      (cadr (div-terms (mul-term-by-all-terms ift tl1)
                       tl2))))
  
  (define (gcd-terms tl1 tl2)
    (define (helper L1 L2)
      (if (empty-termlist? L2)
          L1
          (helper L2 (pseudoremainder-terms L1 L2))))
    (define (coeffs-list L)
      (if (empty-termlist? L)
          null
          (cons (coeff (first-term L))
                (coeffs-list (rest-terms L)))))
    (define (simplify-coeffs L)
      (mul-term-by-all-terms (make-term 0 (/ 1 (apply gcd (coeffs-list L)))) L))
    (simplify-coeffs (helper tl1 tl2)))
  
  (define (gcd-poly p1 p2)
    (let ((v1 (variable p1))
          (v2 (variable p2)))
      (if (same-variable? v1 v2)
          (make-poly v1 (gcd-terms (term-list p1) (term-list p2)))
          (error "GCD-POLY -- polynomials must have the same indeterminate" (list p1 p2)))))

  ; ex 2.97
  (define (reduce-terms L1 L2)
    (define (degree L) (order (first-term L)))
    (let* ((g-c-d (gcd-terms L1 L2))
           (integerizing-factor (expt (coeff (first-term g-c-d)) (+ 1 (- (max (degree L1) (degree L2)) (degree g-c-d)))))
           (ift (make-term 0 integerizing-factor))
           (newL1 (mul-term-by-all-terms ift L1))
           (newL2 (mul-term-by-all-terms ift L2)))
      (list (quotient-terms newL1 g-c-d)
            (quotient-terms newL2 g-c-d))))

  (define (reduce-poly p1 p2)
    (let ((v1 (variable p1))
          (v2 (variable p2)))
      (if (same-variable? v1 v2)
          (let ((result (reduce-terms (term-list p1) (term-list p2))))
            (list (make-poly v1 (car result)) (make-poly v1 (cadr result))))
          (error "REDUCE-POLY -- polynomials must have the same indeterminate" (list p1 p2)))))

  ; public interface
  (define (tag datum) (attach-tag 'polynomial datum))
  (put 'make 'polynomial (lambda (var terms) (tag (make-poly var terms))))
  (put 'variable 'polynomial variable)
  (put 'term-list 'polynomial term-list)
  
  (put 'add '(polynomial polynomial) (lambda (p1 p2) (tag (add-poly p1 p2))))
  (put 'sub '(polynomial polynomial) (lambda (p1 p2) (tag (sub-poly p1 p2))))
  (put 'mul '(polynomial polynomial) (lambda (p1 p2) (tag (mul-poly p1 p2))))
  (put 'div '(polynomial polynomial) (lambda (p1 p2) (map tag (div-poly p1 p2))))

  (put 'equ? '(polynomial polynomial) equ?-poly)
  (put '=zero? '(polynomial) =zero?-poly)
  (put 'negate '(polynomial) (lambda (p) (tag (negate-poly p))))

  (put 'gcd '(polynomial polynomial) (lambda (p1 p2) (tag (gcd-poly p1 p2))))

  (put 'reduce '(polynomial polynomial) (lambda (p1 p2) (map tag (reduce-poly p1 p2))))

  'installed-polynomials-package)

(install-polynomial-package)

(define (make-polynomial var terms) ((get 'make 'polynomial) var terms))
(define (variable p) ((get 'variable 'polynomial) (contents p)))
(define (term-list p) ((get 'term-list 'polynomial) (contents p)))