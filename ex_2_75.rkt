#lang racket
(define (make-from-mag-ang r a)
  (define (dispatch op)
    (cond ((eq? op 'real-part) (* r (cos a)))
          ((eq? op 'imag-part) (* r (sin a)))
          ((eq? op 'mag) r)
          ((eq? op 'ang) a)
          (else (error "Invalid op" op))))
  dispatch)