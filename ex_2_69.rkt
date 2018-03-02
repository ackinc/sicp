#lang racket
(require "./ch_2_huffman_tree_ops.rkt")
(provide generate-huffman-tree)

(define (generate-huffman-tree pairs)
  (successive-merge (make-leaf-set pairs)))

(define (successive-merge pairs)
  (cond ((null? pairs) null)
        ((null? (cdr pairs)) (car pairs))
        (else (successive-merge (adjoin-set (make-code-tree (car pairs) (cadr pairs))
                                            (cddr pairs))))))

;(generate-huffman-tree '((A 4) (B 2) (C 1) (D 1)))