#lang racket
(require "general_arithmetic_ops.rkt")
(require "hash_ops.rkt")
(require "polynomials__term_ops.rkt")
(require "scheme_numbers.package.rkt") ; for =zero? on ordinary numbers
(require "type_tag_helpers.rkt")

(define (install-sparse-polynomial-package)
  (define (the-empty-termlist) null)
  (define (empty-termlist? tl) (null? tl))
  (define (first-term tl) (car tl))
  (define (rest-terms tl) (cdr tl))
  (define (adjoin-term t tl) (if (=zero? (coeff t)) tl (cons t tl)))

  (define (is-appropriate-type tl) ; return #t if length(tl) < 1/2 of highest-order(tl)
    (if (empty-termlist? tl)
        #f
        (< (length tl) (/ (order (first-term tl)) 2))))

  (define (convert dtl) ; dense tl -> sparse tl
    (define first-term (get 'first-term '(dense)))
    (define rest-terms (get 'rest-terms '(dense)))
    (define empty-termlist? (get 'empty-termlist? '(dense)))
    (define (helper dtl)
      (if (empty-termlist? dtl)
          (the-empty-termlist)
          (adjoin-term (first-term dtl) (helper (contents (rest-terms dtl))))))
    (helper dtl))

  (define (convert-to-other-type stl)
    ((get 'convert '(dense)) stl))

  ; public interface
  (define (tag datum) (attach-tag 'sparse datum))

  (put 'the-empty-termlist 'sparse (lambda () (tag (the-empty-termlist))))
  (put 'empty-termlist? '(sparse) empty-termlist?)
  (put 'first-term '(sparse) first-term)
  (put 'rest-terms '(sparse) (lambda (tl) (tag (rest-terms tl))))
  (put 'adjoin-term 'sparse (lambda (t tl) (tag (adjoin-term t tl))))
  (put 'is-appropriate-type '(sparse) is-appropriate-type)
  (put 'convert '(sparse) (lambda (tl) (tag (convert tl))))
  (put 'convert-to-other-type '(sparse) convert-to-other-type)

  (put 'make 'sparse (lambda (tl) (tag tl)))

  'installed-sparse-termlists-package)

(install-sparse-polynomial-package)