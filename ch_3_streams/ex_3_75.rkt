#lang racket
(require "stream_procs.rkt")
(define (sign-change-detector cur prev)
  (cond ((and (< cur 0) (>= prev 0)) -1)
        ((and (>= cur 0) (< prev 0)) 1)
        (else 0)))

(define (make-zero-crossings input-stream prev-avg prev-value)
  (if (stream-null? input-stream)
      the-empty-stream
      (let* ((cur-value (stream-car input-stream))
             (cur-avg (/ (+ cur-value prev-value) 2)))
        (cons-stream (sign-change-detector cur-avg prev-avg)
                     (make-zero-crossings (stream-cdr input-stream) cur-avg cur-value)))))

(define sense-data (make-stream-from-list (list 1 2 1.5 1 0.5 -0.1 -2 -3 -2 -0.5 0.2 3 4)))
(define zero-crossings (make-zero-crossings sense-data 0 0))
(display-stream zero-crossings 20)