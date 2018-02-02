#lang racket
(define (equal? x y)
  (cond ((and (symbol? x) (symbol? y) (eq? x y)) #t)
        ((or (symbol? x) (symbol? y) (not (eq? x y))) #f)
        ((and (null? x) (null? y)) #t)
        ((or (null? x) (null? y) (not (eq? (car x) (car y)))) #f)
        (else (equal? (cdr x) (cdr y)))))