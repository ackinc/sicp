#lang racket
(require "stream_procs.rkt")
(require "integral.rkt")

(define (RLC R L C dt)
  (lambda (vc0 il0)
    (define vc (delayed-integral (delay (scale-stream il (- (/ 1 C)))) vc0 dt))
    (define il (delayed-integral (delay dil) il0 dt))
    (define dil (scale-stream (add-streams vc (scale-stream il (- R))) (/ 1 L)))
    (cons vc il)))

(define circuit (RLC 1 0.2 1 0.1))
(define s (circuit 10 0))