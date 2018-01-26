#lang racket
(define (tree-map proc tree)
  (cond ((null? tree) null)
        ((pair? (car tree)) (cons (tree-map proc (car tree))
                                  (tree-map proc (cdr tree))))
        (else (cons (proc (car tree))
                    (tree-map proc (cdr tree))))))