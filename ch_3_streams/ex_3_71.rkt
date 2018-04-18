#lang racket
(require "stream_procs.rkt")
(require "useful_streams.rkt")
(require "ex_3_70.rkt")

(define (sum-cubes p)
  (define (cube x) (* x x x))
  (+ (cube (car p)) (cube (cadr p))))
(define pairs-ordered-by-sum-cubes (weighted-pairs integers integers sum-cubes))

(define (ramanujam-numbers)
  (define (helper s prev-pair)
    (let* ((cur-pair (stream-car s))
           (prev-weight (sum-cubes prev-pair))
           (cur-weight (sum-cubes cur-pair)))
      (if (= cur-weight prev-weight)
          (cons-stream (list cur-weight prev-pair cur-pair) (helper (stream-cdr s) cur-pair))
          (helper (stream-cdr s) cur-pair))))
  (let ((first (stream-car pairs-ordered-by-sum-cubes))
        (rest (stream-cdr pairs-ordered-by-sum-cubes)))
    (helper rest first)))

(define r (ramanujam-numbers))
(display-stream r 6)