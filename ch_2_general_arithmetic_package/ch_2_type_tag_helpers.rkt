#lang racket
(provide attach-tag type-tag contents)

(define (attach-tag tag datum)
  (cond ((number? datum) datum)
        (else (cons tag datum))))

(define (type-tag object)
  (cond ((number? object) 'scheme-number)
        ((pair? object) (car object))
        (else (error "Bad datum -- TYPE-TAG" object))))

(define (contents object)
  (cond ((number? object) object)
        ((pair? object) (cdr object))
        (else (error "Bad datum -- CONTENTS" object))))