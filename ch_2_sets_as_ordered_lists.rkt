#lang racket
(provide element-of-set? adjoin-set intersection-set union-set)

(define (element-of-set? x set)
  (cond ((null? set) #f)
        ((= x (car set)) #t)
        ((< x (car set)) #f)
        (else (element-of-set? x (cdr set)))))

; ex 2.61
(define (adjoin-set x set)
  (cond ((null? set) (list x))
        ((= x (car set)) set)
        ((< x (car set)) (cons x set))
        (else (cons (car set) (adjoin-set x (cdr set))))))

(define (intersection-set set1 set2)
  (cond ((or (null? set1) (null? set2)) null)
        ((= (car set1) (car set2)) (cons (car set1) (intersection-set (cdr set1) (cdr set2))))
        ((< (car set1) (car set2)) (intersection-set (cdr set1) set2))
        (else (intersection-set set1 (cdr set2)))))

; ex 2.62
(define (union-set set1 set2)
  (cond ((null? set1) set2)
        ((null? set2) set1)
        ((= (car set1) (car set2)) (cons (car set1) (union-set (cdr set1) (cdr set2))))
        ((< (car set1) (car set2)) (cons (car set1) (union-set (cdr set1) set2)))
        (else (cons (car set2) (union-set set1 (cdr set2))))))

(equal? (element-of-set? 1 null) #f)
(equal? (element-of-set? 1 (list 1 2 3)) #t)
(equal? (adjoin-set 1 (list 2 3)) (list 1 2 3))
(equal? (adjoin-set 2 (list 2 3)) (list 2 3))
(equal? (intersection-set (list 1 2 3) (list 4 5 6)) null)
(equal? (intersection-set (list 1 2 3) (list 1 5 6)) (list 1))
(equal? (union-set (list 1 2 3) null) (list 1 2 3))
(equal? (union-set (list 1 2 3) (list 4 5 6)) (list 1 2 3 4 5 6))
(equal? (union-set (list 4 5 6) (list 1 2 3)) (list 1 2 3 4 5 6))
(equal? (union-set (list 1 2 3) (list 1 5 6)) (list 1 2 3 5 6))