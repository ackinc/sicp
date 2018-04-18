#lang racket
(require "stream_procs.rkt")
(define (sign-change-detector cur prev)
  (cond ((and (< cur 0) (>= prev 0)) -1)
        ((and (>= cur 0) (< prev 0)) 1)
        (else 0)))

(define (smooth s)
  (scale-stream (add-streams s (cons-stream 0 s)) 0.5))

(define (make-zero-crossings s prev)
  (if (stream-null? s)
      the-empty-stream
      (let ((cur (stream-car s)))
        (cons-stream (sign-change-detector cur prev)
                     (make-zero-crossings (stream-cdr s) cur)))))

(define sense-data (make-stream-from-list (list 1 2 1.5 1 0.5 -0.1 -2 -3 -2 -0.5 0.2 3 4)))
(define zero-crossings (make-zero-crossings (smooth sense-data) 0))
(display-stream zero-crossings 20)