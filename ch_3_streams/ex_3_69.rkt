#lang racket
(require "stream_procs.rkt")
(require "useful_streams.rkt")
(require "ex_3_67.rkt")
(require "ex_3_70.rkt")

(define (triple-weight t)
  (+ (car t) (cadr t) (caddr t)))

(define (triples s t u)
  (cons-stream (list (stream-car s) (stream-car t) (stream-car u))
               (merge-weighted (stream-map (lambda (x) (cons (stream-car s) x)) (stream-cdr (weighted-pairs t u pair-weight)))
                               (triples (stream-cdr s) (stream-cdr t) (stream-cdr u))
                               triple-weight)))


(define t (triples integers integers integers))
(display-stream t 10)

(define (square x) (* x x))
(define s (stream-filter (lambda (p) (= (+ (square (car p)) (square (cadr p)))
                                        (square (caddr p))))
                         t))
(display-stream s 10)