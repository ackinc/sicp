#lang racket
(define (make-f)
  (let ((n 1))
    (lambda (x)
      (if (> x 0)
          n
          (begin (set! n 0)
                 n)))))

(define f1 (make-f))
(= (+ (f1 0) (f1 1)) 0)

(define f2 (make-f))
(= (+ (f2 1) (f2 0)) 1)