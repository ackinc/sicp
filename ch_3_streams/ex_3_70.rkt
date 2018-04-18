#lang racket
(require "stream_procs.rkt")
(require "useful_streams.rkt")
(provide pair-weight weighted-pairs)

(define (pair-weight p)
  (+ (car p) (cadr p)))

(define (alt-pair-weight p)
  (+ (* 2 (car p)) (* 3 (cadr p)) (* 5 (car p) (cadr p))))

(define (weighted-pairs s t weightfn)
  (cons-stream (list (stream-car s) (stream-car t))
               (merge-weighted (stream-map (lambda (x) (list (stream-car s) x)) (stream-cdr t))
                               (weighted-pairs (stream-cdr s) (stream-cdr t) weightfn)
                               weightfn)))

(define s (weighted-pairs integers integers pair-weight))
(display-stream s 10)

(define t (stream-filter (lambda (p) (let ((i (car p))
                                           (j (cadr p)))
                                       (not (or (even? i)
                                                (even? j)
                                                (= (remainder i 3) 0)
                                                (= (remainder j 3) 0)
                                                (= (remainder i 5) 0)
                                                (= (remainder j 5) 0)))))
                         (weighted-pairs integers integers alt-pair-weight)))
(display-stream t 10)