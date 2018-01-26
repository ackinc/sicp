#lang racket
(define (last-pair l)
  (let ((tail (cdr l)))
    (if (null? tail)
        l
        (last-pair tail))))

(define (last-pair-iter l)
  (define (helper orig cur-elem)
    (if (null? orig)
        (list cur-elem)
        (helper (cdr orig) (car orig))))
  (helper (cdr l) (car l)))