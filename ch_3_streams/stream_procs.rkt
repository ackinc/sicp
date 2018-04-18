#lang racket
(require "delayed_eval.rkt")

(provide cons-stream stream-car stream-cdr stream-null? the-empty-stream
         stream-enumerate-interval stream-for-each stream-filter stream-map stream-ref
         add-streams mul-streams scale-stream
         merge merge-weighted
         display-stream interleave)

(define-syntax-rule (cons-stream a b) (cons a (delay b)))
(define (stream-car s) (car s))
(define (stream-cdr s) (force (cdr s)))

(define (stream-null? s) (null? s))
(define the-empty-stream null)

(define (stream-enumerate-interval a b)
  (if (> a b)
      the-empty-stream
      (cons-stream a (stream-enumerate-interval (+ a 1) b))))

(define (stream-for-each proc s)
  (if (stream-null? s)
      'done
      (begin (proc (stream-car s))
             (stream-for-each proc (stream-cdr s)))))

(define (stream-filter pred s)
  (cond ((stream-null? s) the-empty-stream)
        ((pred (stream-car s)) (cons-stream (stream-car s) (stream-filter pred (stream-cdr s))))
        (else (stream-filter pred (stream-cdr s)))))

(define (stream-map proc . argstreams)
  (if (stream-null? (car argstreams))
      the-empty-stream
      (cons-stream (apply proc (map stream-car argstreams))
                   (apply stream-map (cons proc (map stream-cdr argstreams))))))

(define (stream-ref s n)
  (if (= n 0)
      (stream-car s)
      (stream-ref (stream-cdr s) (- n 1))))

(define (add-streams s1 s2) (stream-map + s1 s2))
(define (mul-streams s1 s2) (stream-map * s1 s2))

(define (scale-stream s factor)
  (stream-map (lambda (x) (* x factor)) s))

(define (merge s1 s2)
  (cond ((stream-null? s1) s2)
        ((stream-null? s2) s1)
        (else (let ((s1car (stream-car s1))
                    (s2car (stream-car s2)))
                (cond ((< s1car s2car) (cons-stream s1car (merge (stream-cdr s1) s2)))
                      ((> s1car s2car) (cons-stream s2car (merge s1 (stream-cdr s2))))
                      (else (cons-stream s1car (merge (stream-cdr s1) (stream-cdr s2)))))))))

; ex 3.70
(define (merge-weighted s1 s2 weight)
  (cond ((stream-null? s1) s2)
        ((stream-null? s2) s1)
        (else (let ((s1car (stream-car s1))
                    (s2car (stream-car s2)))
                (cond ((< (weight s1car) (weight s2car)) (cons-stream s1car (merge-weighted (stream-cdr s1) s2 weight)))
                      (else (cons-stream s2car (merge-weighted s1 (stream-cdr s2) weight))))))))

(define (display-stream s n)
  (if (= n 0)
      'done
      (begin
        (display (stream-car s))
        (newline)
        (display-stream (stream-cdr s) (- n 1)))))

(define (interleave s1 s2)
  (if (stream-null? s1)
      s2
      (cons-stream (stream-car s1) (interleave s2 (stream-cdr s1)))))

;TESTS
;(eq? (stream-car (stream-cdr (stream-filter even? (stream-enumerate-interval 10001 1000000)))) 10004)