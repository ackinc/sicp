#lang racket
(require "stream_procs.rkt")
(define (sign-change-detector cur prev)
  (cond ((and (< cur 0) (>= prev 0)) -1)
        ((and (>= cur 0) (< prev 0)) 1)
        (else 0)))

(define sense-data (make-stream-from-list (list 1 2 1.5 1 0.5 -0.1 -2 -3 -2 -0.5 0.2 3 4)))

(define zero-crossings (stream-map sign-change-detector sense-data (cons-stream 0 sense-data)))
(display-stream zero-crossings 20)