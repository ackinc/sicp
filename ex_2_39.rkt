#lang racket
(require "./ch_2_sequence_ops.rkt") ; append
(require "./ex_2_38.rkt") ; fold-left and fold-right

(define (reverse sequence)
  (fold-left (lambda (x y) (cons y x)) '() sequence))

(define (reverse2 sequence)
  (fold-right (lambda (x y) (append y (list x))) '() sequence))