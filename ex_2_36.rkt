#lang racket
(require (only-in "./ch_2_sequence_ops.rkt"
                  [accumulate-alt accumulate]))
(provide accumulate-n)

(define (accumulate-n op init seqs)
  (if (null? (car seqs))
      null
      (cons (accumulate op init (map car seqs))
            (accumulate-n op init (map cdr seqs)))))