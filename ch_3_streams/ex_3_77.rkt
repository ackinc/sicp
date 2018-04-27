#lang racket
(require "stream_procs.rkt")

; non-implicit defn
(define (integral integrand initial-value dt)
  (cons-stream initial-value
               (if (stream-null? integrand)
                   the-empty-stream
                   (integral (stream-cdr integrand)
                             (+ initial-value (* (stream-car integrand) dt))
                             dt))))

; non-implicit defn with delayed arg
(define (delayed-integral delayed-integrand initial-value dt)
  (cons-stream initial-value
               (let ((integrand (force delayed-integrand)))
                 (if (stream-null? integrand)
                     the-empty-stream
                     (delayed-integral (delay (stream-cdr integrand))
                                       (+ initial-value (* (stream-car integrand) dt))
                                       dt)))))

(define (solve f initial-value dt)
  (define y (delayed-integral (delay fy) initial-value dt))
  (define fy (stream-map f y))
  y)

(stream-ref (solve (lambda (x) x) 1 0.001) 1000) ; e