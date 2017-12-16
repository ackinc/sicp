#lang racket
(provide fixed-point fixed-point-debug)

(define (fixed-point f first-guess)
  (define (close-enough? a b) (< (abs (- a b)) 0.00001))
  (define (try guess)
    (let ((next (f guess)))
      (if (close-enough? guess next)
          next
          (try next))))
  (try first-guess))

(define (fixed-point-debug f first-guess)
  (define (close-enough? a b) (< (abs (- a b)) 0.00001))
  (define (try guess)
    (display guess)
    (newline)
    (let ((next (f guess)))
      (if (close-enough? guess next)
          next
          (try next))))
  (try first-guess))

;TESTING
;(fixed-point cos 1) ; 0.73908
;(fixed-point (lambda (x) (+ (sin x) (cos x))) 1) ; 1.25873
;(fixed-point-debug cos 1) ; 0.73908