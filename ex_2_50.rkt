#lang racket
(require sicp-pict)
(require "./ex_2_49.rkt") ; some primitive painters

(provide flip-vert flip-horiz rotate-ccw-90 rotate-180 rotate-ccw-270)

(define (flip-vert painter)
  ((transform-painter (make-vect 0 1)
                      (make-vect 1 1)
                      (make-vect 0 0)) painter))

(define (flip-horiz painter)
  ((transform-painter (make-vect 1 0)
                      (make-vect 0 0)
                      (make-vect 1 1)) painter))

(define (rotate-ccw-90 painter)
  ((transform-painter (make-vect 1 0)
                      (make-vect 1 1)
                      (make-vect 0 0)) painter))

(define (rotate-180 painter)
  ((transform-painter (make-vect 1 1)
                      (make-vect 0 1)
                      (make-vect 1 0)) painter))

(define (rotate-ccw-270 painter)
  ((transform-painter (make-vect 0 1)
                      (make-vect 0 0)
                      (make-vect 1 1)) painter))