#lang racket
(define (f n)
  (cond ((< n 3) n)
        (else (+ (f (- n 1))
                 (* 2 (f (- n 2)))
                 (* 3 (f (- n 3)))))))

(define (f-iter n)
  (define (f-iter-helper cur cv1 cv2 cv3)
    (define cv (+ cv1 (* 2 cv2) (* 3 cv3)))
    (if (= cur n) cv (f-iter-helper (+ cur 1) cv cv1 cv2)))

  (cond ((< n 3) n)
        (else (f-iter-helper 3 2 1 0))))