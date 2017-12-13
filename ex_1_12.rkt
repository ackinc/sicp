#lang racket
(define (sum-ap n) (/ (* n (+ n 1)) 2))

(define (pascal-row n)
  (define (helper cur)
    (if (>= (sum-ap cur) n)
        cur
        (helper (+ cur 1))))
  (helper 0))

(define (pascal-elem row rowpos)
  (cond ((or (= rowpos 1) (= rowpos row)) 1)
        ((or (< rowpos 1) (> rowpos row)) 0)
        (else (+ (pascal-elem (- row 1) (- rowpos 1))
                 (pascal-elem (- row 1) rowpos)))))

(define (pascal n)
  (define row (pascal-row n))
  (pascal-elem row (- n (- (sum-ap row) row))))