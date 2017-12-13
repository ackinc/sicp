#lang racket
(define (square x) (* x x))

(define (expmod base exp m)
  (cond ((= exp 0) 1)
        ((even? exp) (remainder (square (expmod base (/ exp 2) m)) m))
        (else (remainder (* base (expmod base (- exp 1) m)) m))))

(define (fermat-test-once n a)
  (= (expmod a n n) a))

(define (fermat-test-all n)
  (define (helper cur)
    (cond ((= cur n) #t)
          ((not (fermat-test-once n cur)) #f)
          (else (helper (+ cur 1)))))
  (helper 1))

; known composites
(fermat-test-all 10)
(fermat-test-all 90)
(fermat-test-all 385)
(fermat-test-all 1047)
(fermat-test-all 2937)
(fermat-test-all 594243)

(newline)

; known primes
(fermat-test-all 1009)
(fermat-test-all 1013)
(fermat-test-all 1019)
(fermat-test-all 10037)
(fermat-test-all 100019)
(fermat-test-all 100043)

(newline)

; Carmichael numbers
(fermat-test-all 561)
(fermat-test-all 1105)
(fermat-test-all 1729)
(fermat-test-all 2465)
(fermat-test-all 2821)
(fermat-test-all 6601)