#lang racket
(require "stream_procs.rkt")
(require "ex_3_59.rkt")
(require "ex_3_60.rkt")
(require "ex_3_61.rkt")

(define (div-series s1 s2)
  (if (= (stream-car s2) 0)
      (error "div-series -- denominator series must begin with non-zero constant term!")
      (let ((factor (/ 1 (stream-car s2))))
        (define s2unit (scale-stream s2 factor))
        (scale-stream (mul-series s1 (invert-unit-series s2unit)) factor))))

; TESTS
(define s (div-series cosine-series cosine-series))
(eq? (stream-ref s 0) 1)
(eq? (stream-ref s 5) 0)
(eq? (stream-ref s 10) 0)