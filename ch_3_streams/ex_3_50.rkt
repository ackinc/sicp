#lang racket
(require "stream_procs.rkt")

(define (stream-map proc . argstreams)
  (if (stream-null? (car argstreams))
      the-empty-stream
      (cons-stream (apply proc (map stream-car argstreams))
                   (apply stream-map (cons proc (map stream-cdr argstreams))))))

; TESTS
(define s (stream-map + (stream-enumerate-interval 1 10) (stream-enumerate-interval 11 20) (stream-enumerate-interval 21 30)))
(eq? (stream-ref s 0) 33)
(eq? (stream-ref s 3) 42)
(eq? (stream-ref s 6) 51)
(eq? (stream-ref s 9) 60)