#lang racket
(require "apply_generic.rkt")
(require "general_arithmetic_ops.rkt")
(require "hash_ops.rkt")
(require "polynomials__term_ops.rkt")
(require "scheme_numbers.package.rkt") ; for =zero? on ordinary numbers
(require "type_tag_helpers.rkt")

(define (install-dense-polynomial-package)  
  (define (the-empty-termlist) null)
  (define (empty-termlist? tl) (null? tl))
  (define (first-term tl) (make-term (sub (length tl) 1) (car tl)))
  (define (rest-terms tl) (cdr tl))
  (define (adjoin-term t tl)
    (let ((coeff-t (coeff t))
          (order-t (order t))
          (order-tl (if (empty-termlist? tl) -1 (order (first-term tl)))))
      (cond ((=zero? coeff-t) tl)
            ((> (- order-t order-tl) 1) (adjoin-term t (cons 0 tl)))
            (else (cons coeff-t tl)))))

  (define (is-appropriate-type tl) ; if # 0s in tl is <= half length of tl, return #t, else #f
    (if (empty-termlist? tl)
        #t
        (let ((num-zeroes (foldl (lambda (coeff acc) (if (= coeff 0) (+ acc 1) acc)) 0 tl)))
          (<= num-zeroes (/ (length tl) 2)))))

  (define (convert stl) ; sparse tl -> dense tl
    (define first-term (get 'first-term '(sparse)))
    (define rest-terms (get 'rest-terms '(sparse)))
    (define empty-termlist? (get 'empty-termlist? '(sparse)))
    (define (helper stl) ; this helper is to avoid redefining "first-term" and other functions above in the recursive calls
      (if (empty-termlist? stl)
          (the-empty-termlist)
          (adjoin-term (first-term stl) (helper (contents (rest-terms stl))))))
    (helper stl))

  (define (convert-to-other-type dtl)
    ((get 'convert '(sparse)) dtl))

  ; public interface
  (define (tag datum) (attach-tag 'dense datum))

  (put 'the-empty-termlist 'dense (lambda () (tag (the-empty-termlist))))
  (put 'empty-termlist? '(dense) empty-termlist?)
  (put 'first-term '(dense) first-term)
  (put 'rest-terms '(dense) (lambda (tl) (tag (rest-terms tl))))
  (put 'adjoin-term 'dense (lambda (t tl) (tag (adjoin-term t tl))))
  (put 'is-appropriate-type '(dense) is-appropriate-type)
  (put 'convert '(dense) (lambda (tl) (tag (convert tl))))
  (put 'convert-to-other-type '(dense) convert-to-other-type)

  (put 'make 'dense (lambda (tl) (tag tl)))

  'installed-dense-termlists-package)

(install-dense-polynomial-package)