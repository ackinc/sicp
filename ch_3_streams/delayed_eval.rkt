#lang racket
(provide delay force)

(define (memo-proc proc)
  (let ((already-run? #f)
        (result #f))
    (lambda ()
      (if already-run?
          result
          (begin (set! already-run? #t)
                 (set! result (proc))
                 result)))))

(define-syntax-rule (delay x) (memo-proc (lambda () x)))

(define (force p) (p))