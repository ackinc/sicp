#lang racket
(require "./ch_1_prime.rkt")
(require "./ch_1_timed_prime_test.rkt")

(define (test n)
  (timed-prime-test slower-fermat-prime? n))

(test 1009) ; 2ms
(test 1013)
(test 1019)

(test 10007) ; 130ms
(test 10009)
(test 10037)

(test 100003) ; 5000ms
(test 100019)
(test 100043)

; slower-fermat-prime calculates the squares of massive numbers
; time complexity arithmetic ops is proportional to #bits of operands