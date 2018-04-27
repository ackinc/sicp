#lang racket
(require "stream_procs.rkt")

(define random-init 0)
(define (rand-update x) (+ x 1))

(define (rng continue-val request-stream)
  (define (constructor start-val)
    (cons-stream start-val (rng (rand-update start-val) (stream-cdr request-stream))))
  (if (stream-null? request-stream)
      the-empty-stream
      (let ((first-request (stream-car request-stream)))
        (cond ((eq? first-request 'generate) (constructor continue-val))
              
              ((and (pair? first-request) (eq? (car first-request) 'reset))
               (constructor (cdr first-request)))

              (else (error "rng -- bad request" first-request))))))

(define (RNG request-stream) (rng random-init request-stream))

(define rnums (RNG (make-stream-from-list (list 'generate 'generate (cons 'reset 5)
                                                'generate 'generate (cons 'reset 0)
                                                'generate (cons 'reset 10) 'generate))))

(display-stream rnums 20)