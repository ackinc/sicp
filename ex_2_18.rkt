#lang racket
(define (reverse l)
  (define (helper orig result)
    (if (null? orig)
        result
        (helper (cdr orig) (cons (car orig) result))))
  (helper l '()))

(define (append l1 l2)
  (if (null? l1)
      l2
      (cons (car l1) (append (cdr l1) l2))))

(define (reverse-recur l)
  (if (null? l)
      l
      (append (reverse-recur (cdr l)) (list (car l)))))