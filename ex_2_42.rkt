#lang racket
(require "./ch_2_sequence_ops.rkt")

(define (queens board-size)
  (define (queen-cols k)
    (if (= k 0)
        (list empty-board)
        (filter (lambda (arrangement) (safe? arrangement))
                (flatmap (lambda (arrangement)
                           (map (lambda (new-row) (adjoin-position new-row k arrangement))
                                (enumerate-interval 1 board-size)))
                         (queen-cols (- k 1))))))
  (queen-cols board-size))

(define empty-board '())

; an arrangement is a list of numbers representing the row-pos of each queen
; an arrangement (list 1 3 5) means the first-col queen is in row 5,
;   second-col queen is in row 3,
;   and third-col queen is in row 1

(define (adjoin-position new-row k arrangement)
  (cons new-row arrangement))

; this proc checks that the queen in the first position (kth-row where k is length of arrangement) is safe from other queens
(define (safe? arrangement)
  (define cur-queen-row (car arrangement))
  (define (helper cur-offset rest-of-queens)
    (if (null? rest-of-queens)
        #t
        (and (not (= (car rest-of-queens) cur-queen-row))
             (not (= (+ (car rest-of-queens) cur-offset) cur-queen-row))
             (not (= (- (car rest-of-queens) cur-offset) cur-queen-row))
             (helper (+ cur-offset 1) (cdr rest-of-queens)))))
  (helper 1 (cdr arrangement)))