#lang racket
(provide attach-tag type-tag contents)

(define (attach-tag tag datum) (cons tag datum))

(define (type-tag object)
  (if (pair? object)
      (car object)
      (error "Bad datum -- TYPE-TAG" object)))

(define (contents object)
  (if (pair? object)
      (cdr object)
      (error "Bad datum -- TYPE-TAG" object)))