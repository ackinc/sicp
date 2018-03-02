#lang racket
(require "./ch_2_huffman_tree_ops.rkt")
(require (only-in "./ex_2_67.rkt"
                  sample-tree))

(provide encode)

(define (encode message tree)
  (if (null? message)
      null
      (append (encode-symbol (car message) tree)
              (encode (cdr message) tree))))

(define (contains x set)
  (cond ((null? set) #f)
        ((eq? (car set) x) #t)
        (else (contains x (cdr set)))))

(define (encode-symbol symbol tree)
  (cond ((not (contains symbol (symbols tree))) (error "invalid symbol -- ENCODE-SYMBOL" symbol))
        ((leaf? tree) null)
        (else (let ((lb (left-branch tree))
                    (rb (right-branch tree)))
                (if (contains symbol (symbols lb))
                    (cons 0 (encode-symbol symbol lb))
                    (cons 1 (encode-symbol symbol rb)))))))

;(encode '(A D A B B C A) sample-tree)