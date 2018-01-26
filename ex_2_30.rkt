#lang racket
(define (square-tree tree)
  (cond ((null? tree) null)
        ((number? tree) (* tree tree))
        (else (cons (square-tree (car tree))
                    (square-tree (cdr tree))))))

(define (square-tree2 tree)
  (map (lambda (x)
         (if (number? x)
             (* x x)
             (square-tree2 x)))
       tree))