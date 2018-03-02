#lang racket
(require (only-in "./ch_2_list_operations.rkt"
                  append))

(provide make-leaf leaf? symbol-leaf weight-leaf
         make-code-tree left-branch right-branch symbols weight
         decode
         adjoin-set make-leaf-set)

; functions to manipulate leaf nodes
(define (make-leaf symbol weight)
  (list 'leaf symbol weight))

(define (leaf? node) (eq? (car node) 'leaf))

(define (symbol-leaf x) (cadr x))

(define (weight-leaf x) (caddr x))

; functions to manipulate a tree
(define (make-code-tree left right)
  (list left
        right
        (append (symbols left) (symbols right))
        (+ (weight left) (weight right))))

(define (left-branch tree) (car tree))

(define (right-branch tree) (cadr tree))

; "generic" functions that work with both trees and leaf nodes
(define (symbols node)
  (if (leaf? node)
      (list (symbol-leaf node))
      (caddr node)))

(define (weight node)
  (if (leaf? node)
      (weight-leaf node)
      (cadddr node)))

; converts a sequence of bits into a message
(define (decode bits tree)
  (define (choose-next-branch bit branch)
    (if (= bit 0) (left-branch branch) (right-branch branch)))
  (define (decode-helper bits current-branch)
    (if (null? bits)
        null
        (let ((next-branch (choose-next-branch (car bits) current-branch)))
          (if (leaf? next-branch)
              (cons (symbol-leaf next-branch) (decode-helper (cdr bits) tree))
              (decode-helper (cdr bits) next-branch)))))
  (decode-helper bits tree))

(define (adjoin-set x set)
  (cond ((null? set) (list x))
        ((<= (weight x) (weight (car set))) (cons x set))
        (else (cons (car set) (adjoin-set x (cdr set))))))

(define (make-leaf-set pairs)
  (if (null? pairs)
      null
      (let ((pair (car pairs)))
        (adjoin-set (make-leaf (car pair) (cadr pair))
                    (make-leaf-set (cdr pairs))))))

; TESTING
;(make-leaf-set '((A 4) (B 2) (C 1) (D 1))); '((leaf C 1) (leaf D 1) (leaf B 2) (leaf A 4))