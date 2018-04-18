#lang racket
(require "stream_procs.rkt")
(require "useful_streams.rkt")
(require "ex_3_70.rkt")

(define (sum-squares p)
  (define (square x) (* x x))
  (+ (square (car p)) (square (cadr p))))

(define pairs-ordered-by-sum-squares (weighted-pairs integers integers sum-squares))

(define (numbers)
  (define (helper s prev1 prev2)
    (let* ((cur (stream-car s))
           (prev-weight1 (sum-squares prev1))
           (prev-weight2 (sum-squares prev2))
           (cur-weight (sum-squares cur)))
      (if (= cur-weight prev-weight1 prev-weight2)
          (cons-stream (list cur-weight cur prev1 prev2) (helper (stream-cdr s) cur prev1))
          (helper (stream-cdr s) cur prev1))))
  (let ((first (stream-car pairs-ordered-by-sum-squares))
        (second (stream-car (stream-cdr pairs-ordered-by-sum-squares)))
        (rest (stream-cdr (stream-cdr pairs-ordered-by-sum-squares))))
    (helper rest first second)))

(define s (numbers))
(display-stream s 10)