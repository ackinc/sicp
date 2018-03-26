#lang racket
(require (only-in "./ex_3_13.rkt"
                  make-cycle))

(define (memq-m item x)
  (cond ((null? x) #f)
        ((eq? item (mcar x)) x)
        (else (memq-m item (mcdr x)))))

(define (check-loop x)
  (define seen null)
  (define (helper x)
    (cond ((not (mpair? x)) #f)
          ((memq-m x seen) #t)
          (else (begin (set! seen (mcons x seen))
                       (helper (mcdr x))))))
  (helper x))

; TESTS
(eq? (check-loop null) #f)
(eq? (check-loop (mcons 1 null)) #f)
(eq? (check-loop (mcons 1 (mcons 2 null))) #f)

(eq? (check-loop (make-cycle (mcons 1 null))) #t)