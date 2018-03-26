#lang racket
(define (append x y)
  (if (null? x) y (mcons (mcar x) (append (mcdr x) y))))

(define (last-pair L)
  (cond ((null? L) (error "last-pair -- arg cannot be empty list"))
        ((null? (mcdr L)) L)
        (else (last-pair (mcdr L)))))
(define (append! mx y)
  (set-mcdr! (last-pair mx) y))

(define x (mcons 'a (mcons 'b null)))
(define y (mcons 'c (mcons 'd null)))

(define z (append x y))
(mcdr x)

(define w (append! x y))
(mcdr x)