#lang racket
(require (only-in "ch_3_concurrency_control.rkt"
                  make-serializer))

(provide incrementer)

(define (incrementer)
  (let ((cur 0)
        (serializer (make-serializer)))
    (define (next!)
      (set! cur (+ cur 1))
      cur)
    (define (reset!) (set! cur 0))
    (define (dispatch m)
      (cond ((eq? m 'next!) ((serializer next!)))
            ((eq? m 'reset! ((serializer reset!))))
            (else (error "incrementer -- unknown message" m))))
    dispatch))