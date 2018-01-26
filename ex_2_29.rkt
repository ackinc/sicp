#lang racket
; binary mobiles

(define (make-mobile left right) (list left right))
(define (make-branch length structure) (list length structure))

; 2.29 a
(define (left-branch mobile) (car mobile))
(define (right-branch mobile) (cadr mobile))
(define (branch-length branch) (car branch))
(define (branch-structure branch) (cadr branch))

; 2.29 b
(define (branch-weight branch)
  (let ((bs (branch-structure branch)))
    (if (number? bs)
        bs
        (total-weight bs))))

(define (total-weight mobile)
  (+ (branch-weight (left-branch mobile))
     (branch-weight (right-branch mobile))))

; 2.29 c
(define (balanced? mobile)
  (let ((lb (left-branch mobile))
        (rb (right-branch mobile)))
    (and (or (number? (branch-structure lb)) (balanced? (branch-structure lb)))
         (or (number? (branch-structure rb)) (balanced? (branch-structure rb)))
         (= (* (branch-weight lb) (branch-length lb))
            (* (branch-weight rb) (branch-length rb))))))

; 2.29 d
;(define (right-branch mobile) (cdr mobile))
;(define (branch-structure branch) (cdr branch))

; TESTS
(total-weight (make-mobile (make-branch 10 10) (make-branch 5 (make-mobile (make-branch 1 1) (make-branch 2 2))))) ; 13
(balanced? (make-mobile (make-branch 10 10) (make-branch 5 (make-mobile (make-branch 1 1) (make-branch 2 2))))) ; #f
(balanced? (make-mobile (make-branch 10 10) (make-branch 20 5))) ; #t
(balanced? (make-mobile (make-branch 10 10) (make-branch 20 (make-mobile (make-branch 5 2.5) (make-branch 5 2.5))))) ; #t