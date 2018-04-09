#lang racket
(provide for-each-except)

(define (for-each-except exception L proc)
  (cond ((null? L) 'ok)
        ((eq? exception (car L)) (for-each-except exception (cdr L) proc))
        (else (begin (proc (car L))
                     (for-each-except exception (cdr L) proc)))))