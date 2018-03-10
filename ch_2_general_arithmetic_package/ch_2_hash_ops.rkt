#lang racket
(provide get put
         get-coercion put-coercion)

(define operations (make-hash))
(define coercion (make-hash))

(define (get op types)
  (hash-ref operations (cons op types) #f))

(define (put op types fn)
  (hash-set! operations (cons op types) fn))

(define (get-coercion types)
  (hash-ref coercion types #f))

(define (put-coercion types fn)
  (hash-set! coercion types fn))