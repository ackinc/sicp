#lang racket
(require "stream_procs.rkt")

(define (integral integrand initial-value step)
  (define result (cons-stream initial-value
                              (add-streams (scale-stream integrand step)
                                           result)))
  result)

(define (RC R C dt)
  (lambda (i v0)
    (let ((Ri (scale-stream i R))
          (ibyC (scale-stream i (/ 1 C))))
      (add-streams (integral ibyC v0 dt) Ri))))