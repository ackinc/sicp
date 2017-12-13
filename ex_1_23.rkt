#lang racket
(require "./ch_1_prime.rkt")
(require "./ch_1_timed_prime_test.rkt")

(define (test n)
  (timed-prime-test faster-prime? n))

(test 1000000007) ; .5ms
(test 1000000009)
(test 1000000021)

(test 10000000019) ; 1.5ms
(test 10000000033)
(test 10000000061)

(test 1000000000039) ; 15ms
(test 1000000000061)
(test 1000000000063)

(test 10000000000037) ; 50ms
(test 10000000000051)
(test 10000000000099)

(test 1000000000000037) ; 480ms
(test 1000000000000091)
(test 1000000000000159)

(test 10000000000000061) ; 1500ms
(test 10000000000000069)
(test 10000000000000079)