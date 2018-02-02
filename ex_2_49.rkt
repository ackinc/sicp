#lang racket
(require sicp-pict)

(provide outline-painter x-painter diamond-painter wave-painter)

(define outline-painter
  (segments->painter (list (make-segment (make-vect 0 0) (make-vect .99 0))
                           (make-segment (make-vect .99 0) (make-vect .99 .99))
                           (make-segment (make-vect .99 .99) (make-vect 0 .99))
                           (make-segment (make-vect 0 .99) (make-vect 0 0)))))

(define x-painter
  (segments->painter (list (make-segment (make-vect 0 0) (make-vect .99 .99))
                           (make-segment (make-vect .99 0) (make-vect 0 .99)))))

(define diamond-painter
  (segments->painter (list (make-segment (make-vect 0.5 0) (make-vect 0.99 0.5))
                           (make-segment (make-vect 0.99 0.5) (make-vect 0.5 0.99))
                           (make-segment (make-vect 0.5 0.99) (make-vect 0 0.5))
                           (make-segment (make-vect 0 0.5) (make-vect 0.5 0)))))

(define wave-painter
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
                           (make-segment (make-vect 0.6 0.45) (make-vect 0.99 0.15)))))