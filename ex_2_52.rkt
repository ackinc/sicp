#lang racket
(require sicp-pict)
(require "./ex_2_49.rkt") ; wave-painter
(require (only-in "./ex_2_50.rkt"
                  rotate-180)) ; rotate-180
(require "./ex_2_51.rkt") ; below, beside

(define (right-split painter n)
  (if (= n 0)
      painter
      (beside painter (right-split painter (- n 1)))))

(define (up-split painter n)
  (if (= n 0)
      painter
      (below painter (up-split painter (- n 1)))))

(define (corner-split painter n)
  (if (= n 0)
      painter
      (let ((rsp (right-split painter (- n 1)))
            (usp (up-split painter (- n 1))))
        (below (beside painter
                       (below rsp rsp))
               (beside (beside usp usp)
                       (corner-split painter (- n 1)))))))

; b
(define (corner-split-alt painter n)
  (if (= n 0)
      painter
      (let ((rsp (right-split painter (- n 1)))
            (usp (up-split painter (- n 1))))
        (below (beside painter
                       rsp)
               (beside usp
                       (corner-split-alt painter (- n 1)))))))

(define (square-of-four tl tr bl br)
  (lambda (painter)
    (below (beside (bl painter) (br painter)) (beside (tl painter) (tr painter)))))

(define (square-limit painter n)
  ((square-of-four flip-horiz identity rotate-180 flip-vert) (corner-split painter n)))

; c
(define (square-limit-alt painter n)
  ((square-of-four flip-vert rotate-180 identity flip-horiz) (corner-split painter n)))

; a
(define wave-painter-alt
  (segments->painter (list (make-segment (make-vect 0 0.85) (make-vect 0.2 0.6))
                           (make-segment (make-vect 0.2 0.6) (make-vect 0.3 0.65))
                           (make-segment (make-vect 0.3 0.65) (make-vect 0.45 0.65))
                           (make-segment (make-vect 0.45 0.65) (make-vect 0.40 0.85))
                           (make-segment (make-vect 0.4 0.85) (make-vect 0.45 0.99))
                           (make-segment (make-vect 0.6 0.99) (make-vect 0.65 0.85))
                           (make-segment (make-vect 0.65 0.85) (make-vect 0.6 0.65))
                           (make-segment (make-vect 0.6 0.65) (make-vect 0.7 0.65))
                           (make-segment (make-vect 0.7 0.65) (make-vect 0.99 0.4))
                           (make-segment (make-vect 0 0.65) (make-vect 0.2 0.45))
                           (make-segment (make-vect 0.2 0.45) (make-vect 0.3 0.6))
                           (make-segment (make-vect 0.3 0.6) (make-vect 0.4 0.5))
                           (make-segment (make-vect 0.4 0.5) (make-vect 0.3 0))
                           (make-segment (make-vect 0.4 0) (make-vect 0.5 0.3))
                           (make-segment (make-vect 0.5 0.3) (make-vect 0.6 0))
                           (make-segment (make-vect 0.7 0) (make-vect 0.6 0.45))
                           (make-segment (make-vect 0.6 0.45) (make-vect 0.99 0.15))
                           (make-segment (make-vect 0.48 0.75) (make-vect 0.52 0.7))
                           (make-segment (make-vect 0.52 0.7) (make-vect 0.56 0.75)))))