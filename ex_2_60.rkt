#lang racket
; same
(define (element-of-set? x set)
  (cond ((null? set) #f)
        ((equal? x (car set)) #t)
        (else (element-of-set? x (cdr set)))))

; O(1) instead of O(n)
(define (adjoin-set x set) (cons x set))

; same
(define (intersection-set set1 set2)
  (cond ((or (null? set1) (null? set2)) null)
        ((element-of-set? (car set1) set2) (cons (car set1) (intersection-set (cdr set1) set2)))
        (else (intersection-set (cdr set1) set2))))

; O(m) instead of O(mn)
(define (union-set set1 set2)
  (cond ((null? set1) set2)
        (else (adjoin-set (car set1) (union-set (cdr set1) set2)))))

; TESTS
(equal? (element-of-set? 'x '()) #f)
(equal? (element-of-set? 'x '(x y z)) #t)
(equal? (adjoin-set 'x '(y z)) '(x y z))
(equal? (adjoin-set 'y '(y z)) '(y y z))
(equal? (intersection-set '(x y z) '(a b c)) null)
(equal? (intersection-set '(x y z) '(x b c)) '(x))
(equal? (union-set '(x y z) '()) '(x y z))
(equal? (union-set '(x y z) '(a b c)) '(x y z a b c))
(equal? (union-set '(x y z) '(x b c)) '(x y z x b c))