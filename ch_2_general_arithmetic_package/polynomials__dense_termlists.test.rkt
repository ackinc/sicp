#lang racket
(require "apply_generic.rkt")
(require "hash_ops.rkt")
(require "polynomials__dense_termlists.package.rkt")
(require "polynomials__sparse_termlists.package.rkt") ; for convert
(require "polynomials__term_ops.rkt")
(require "type_tag_helpers.rkt")
(require (only-in "../ch_2_list_operations.rkt"
                  list-equal?))

; METHODS USED IN TESTS
(define the-empty-termlist (get 'the-empty-termlist 'dense))
(define (make terms) ((get 'make 'dense) terms))
(define (first-term tl) (apply-generic 'first-term tl))
(define (rest-terms tl) (apply-generic 'rest-terms tl))
(define (empty-termlist? tl) (apply-generic 'empty-termlist? tl))
(define (adjoin-term t tl) ((get 'adjoin-term 'dense) t (contents tl)))
(define (is-appropriate-type tl) (apply-generic 'is-appropriate-type tl))
(define (convert stl) ((get 'convert '(dense)) (contents stl)))
(define (convert-to-other-type dtl) (apply-generic 'convert-to-other-type dtl))

; TESTS
(display "make, first-term, and rest-terms")(newline)
(define tl (make '(1 0 3)))
(list-equal? tl '(dense 1 0 3))
(list-equal? (first-term tl) '(2 1))
(list-equal? (rest-terms (rest-terms tl)) '(dense 3))

(display "empty termlist")(newline)
(list-equal? (the-empty-termlist) '(dense))
(eq? (empty-termlist? (rest-terms (rest-terms (rest-terms tl)))) #t)

(display "adjoin-term")(newline)
(list-equal? (adjoin-term (make-term 1 0) (the-empty-termlist)) (the-empty-termlist))
(list-equal? (adjoin-term (make-term 3 5) (the-empty-termlist)) '(dense 5 0 0 0))
(list-equal? (adjoin-term (make-term 3 5) (adjoin-term (make-term 0 1) (the-empty-termlist))) '(dense 5 0 0 1))

(display "is-appropriate-type")(newline)
(eq? (is-appropriate-type (make '(3 1 3 -4 0 1))) #t)
(eq? (is-appropriate-type (make '(13 0 0 0 5 0 0 2 0 0 0 0 0 1))) #f)

(display "convert")(newline)
(list-equal? (convert '(sparse (12 13) (9 5) (6 2) (0 1))) (make '(13 0 0 5 0 0 2 0 0 0 0 0 1)))
(list-equal? (convert-to-other-type (make '(13 0 0 5 0 0 2 0 0 0 0 0 1))) '(sparse (12 13) (9 5) (6 2) (0 1)))
