#lang racket
(require (only-in "./ex_3_13.rkt"
                  make-cycle))

(define (check-loop x)
  (define (iter a b)
    (cond ((or (null? a) (null? b)) #f)
          ((eq? a b) #t)
          (else (let ((next-a (mcdr a))
                      (next-b_ (mcdr b)))
                  (if (null? next-b_)
                      #f
                      (let ((next-b (mcdr next-b_)))
                        (iter next-a next-b)))))))
  (if (or (null? x) (null? (mcdr x)))
      #f
      (iter x (mcdr x))))
          

; TESTS
(eq? (check-loop null) #f)
(eq? (check-loop (mcons 1 null)) #f)
(eq? (check-loop (mcons 1 (mcons 2 null))) #f)

(eq? (check-loop (make-cycle (mcons 1 null))) #t)

(define x (make-cycle (mcons 1 (mcons 2 (mcons 3 null)))))
(eq? (check-loop x) #t)
(eq? (check-loop (mcons 5 (mcons 4 x))) #t)