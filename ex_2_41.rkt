#lang racket
(require "./ex_2_40.rkt")
(require "./ch_2_sequence_ops.rkt")

(define (unique-triplets n)
  (flatmap (lambda (pair)
             (map (lambda (k) (list (car pair) (cadr pair) k))
                  (enumerate-interval 1 (- (cadr pair) 1))))
           (unique-pairs n)))

(define (triples-summing-to s n)
  (filter (lambda (triple) (= (+ (car triple) (cadr triple) (caddr triple)) s))
          (unique-triplets n)))