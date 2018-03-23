#lang racket
; dummy methods to help with testing rand
(define random-init 0)
(define (rand-update x) (+ x 1))

(define (rand)
  (define x random-init)
  (define (reset) (lambda (new-val) (set! x new-val)))
  (define (generate)
    (set! x (rand-update x))
    x)
  (define (dispatch m)
    (cond ((eq? m 'reset) (reset))
          ((eq? m 'generate) (generate))
          (else "Invalid operation")))
  dispatch)

; TESTING
(define A (rand))
(define B (rand))

(= (A 'generate) 1)
(= (A 'generate) 2)
((A 'reset) 0)
(= (A 'generate) 1)
(= (A 'generate) 2)

(= (B 'generate) 1)
((B 'reset) 20)
(= (B 'generate) 21)