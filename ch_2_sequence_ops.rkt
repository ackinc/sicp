#lang racket
(provide append enumerate-interval enumerate-tree map filter accumulate accumulate-alt flatmap)

(define (append s1 s2)
  (if (null? s1)
      s2
      (cons (car s1) (append (cdr s1) s2))))

(define (enumerate-interval low high)
  (if (> low high)
      null
      (cons low (enumerate-interval (+ low 1) high))))

(define (enumerate-tree tree)
  (cond ((null? tree) null)
        ((not (pair? tree)) (list tree))
        (else (append (enumerate-tree (car tree))
                      (enumerate-tree (cdr tree))))))

(define (map proc sequence)
  (cond ((null? sequence) null)
        (else (cons (proc (car sequence))
                    (map proc (cdr sequence))))))

(define (filter pred? sequence)
  (cond ((null? sequence) null)
        ((pred? (car sequence)) (cons (car sequence) (filter pred? (cdr sequence))))
        (else (filter pred? (cdr sequence)))))

; tail-recursive ; iterative process ; processes seq left-to-right
(define (accumulate op initial sequence)
  (if (null? sequence)
      initial
      (accumulate op (op (car sequence) initial) (cdr sequence))))

; recursive process ; processes seq right-to-left
(define (accumulate-alt op initial sequence)
  (if (null? sequence)
      initial
      (op (car sequence) (accumulate-alt op initial (cdr sequence)))))

(define (flatmap proc seq)
  (accumulate-alt append null (map proc seq)))