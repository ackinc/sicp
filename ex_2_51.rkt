#lang racket
(require sicp-pict)
(require (only-in "./ex_2_50.rkt"
                  rotate-ccw-90
                  rotate-ccw-270))

(define (beside painter1 painter2)
  (let ((left-part ((transform-painter (make-vect 0 0)
                                       (make-vect 0.5 0)
                                       (make-vect 0 1)) painter1))
        (right-part ((transform-painter (make-vect 0.5 0)
                                        (make-vect 1 0)
                                        (make-vect 0.5 1)) painter2)))
    (lambda (frame)
      (left-part frame)
      (right-part frame))))

(define (below painter1 painter2)
  (let ((bot-part ((transform-painter (make-vect 0 0)
                                      (make-vect 1 0)
                                      (make-vect 0 0.5)) painter1))
        (top-part ((transform-painter (make-vect 0 0.5)
                                      (make-vect 1 0.5)
                                      (make-vect 0 1)) painter2)))
    (lambda (frame)
      (top-part frame)
      (bot-part frame))))

(define (below2 painter1 painter2)
  (let ((p1-r270 (rotate-ccw-270 painter1))
        (p2-r270 (rotate-ccw-270 painter2)))
    (rotate-ccw-90 (beside p1-r270 p2-r270))))