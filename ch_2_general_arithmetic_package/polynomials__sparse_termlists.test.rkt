#lang racket
(require "apply_generic.rkt")
(require "hash_ops.rkt")
(require "polynomials__dense_termlists.package.rkt") ; for convert
(require "polynomials__sparse_termlists.package.rkt")
(require "polynomials__term_ops.rkt")
(require "type_tag_helpers.rkt")
(require (only-in "../ch_2_list_operations.rkt"
                  list-equal?))

(define the-empty-termlist (get 'the-empty-termlist 'sparse))
(define (make terms) ((get 'make 'sparse) terms))
(define (first-term tl) (apply-generic 'first-term tl))
(define (rest-terms tl) (apply-generic 'rest-terms tl))
(define (empty-termlist? tl) (apply-generic 'empty-termlist? tl))
(define (adjoin-term t tl) ((get 'adjoin-term 'sparse) t (contents tl)))
(define (is-appropriate-type tl) (apply-generic 'is-appropriate-type tl))
(define (convert dtl) ((get 'convert '(sparse)) (contents dtl)))
(define (convert-to-other-type stl) (apply-generic 'convert-to-other-type stl))

; TESTS
(display "make, first-term, and rest-terms")(newline)
(define tl (make '((3 1) (2 1) (0 1))))
(list-equal? tl '(sparse (3 1) (2 1) (0 1)))
(list-equal? (first-term tl) '(3 1))
(list-equal? (rest-terms (rest-terms tl)) '(sparse (0 1)))

(display "empty termlist")(newline)
(list-equal? (the-empty-termlist) '(sparse))
(eq? (empty-termlist? (rest-terms (rest-terms (rest-terms tl)))) #t)

(display "adjoin-term")(newline)
(list-equal? (adjoin-term (make-term 1 0) (the-empty-termlist)) (the-empty-termlist))
(list-equal? (adjoin-term (make-term 3 0) (adjoin-term (make-term 0 1) (the-empty-termlist))) '(sparse (0 1)))
(list-equal? (adjoin-term (make-term 3 5) (adjoin-term (make-term 0 1) (the-empty-termlist))) '(sparse (3 5) (0 1)))

(display "is-appropriate-type")(newline)
(eq? (is-appropriate-type (make '((100 1) (0 3)))) #t)
(eq? (is-appropriate-type (make '((3 1) (2 3) (1 9) (0 4)))) #f)

(display "convert")(newline)
(list-equal? (convert '(dense 13 0 0 5 0 0 2 0 0 0 0 0 1)) (make '((12 13) (9 5) (6 2) (0 1))))
(list-equal? (convert-to-other-type (make '((12 13) (9 5) (6 2) (0 1)))) '(dense 13 0 0 5 0 0 2 0 0 0 0 0 1))