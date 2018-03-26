#lang racket
(define (memq-m item x)
  (cond ((null? x) #f)
        ((eq? item (mcar x)) x)
        (else (memq-m item (mcdr x)))))

(define (count-pairs x)
  (define seen null)
  (define (helper x)
    (cond ((not (mpair? x)) 0)
          ((memq-m x seen) 0)
          (else (begin (set! seen (mcons x seen))
                       (+ (helper (mcar x))
                          (helper (mcdr x))
                          1)))))
  (helper x))

; ex 3.16
(define (count-pairs-naive x)
  (if (not (mpair? x))
      0
      (+ (count-pairs-naive (mcar x))
         (count-pairs-naive (mcdr x))
         1)))

; TESTS
(= (count-pairs '()) 0)
(= (count-pairs (mcons 'a null)) 1)
(= (count-pairs (mcons 'a (mcons 'b (mcons 'c (mcons 'd null))))) 4)

(display "Non-straightforward list tests")(newline)
(define a (mcons 3 null))
(define b (mcons a a))
(define c (mcons 1 b))
(define d (mcons b b))

(display "P4 test")(newline)
(= (count-pairs-naive c) 4)
(= (count-pairs c) 3)

(display "P7 test")(newline)
(= (count-pairs-naive d) 7)
(= (count-pairs d) 3)
