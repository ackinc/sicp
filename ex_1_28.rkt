#lang racket
(require "./ch_1_prime.rkt")

; known composites
(miller-rabin-prime? 10)
(miller-rabin-prime? 90)
(miller-rabin-prime? 385)
(miller-rabin-prime? 1047)
(miller-rabin-prime? 2937)
(miller-rabin-prime? 594243)

(newline)

; known primes
(miller-rabin-prime? 1009)
(miller-rabin-prime? 1013)
(miller-rabin-prime? 1019)
(miller-rabin-prime? 10037)
(miller-rabin-prime? 100019)
(miller-rabin-prime? 100043)

(newline)

; Carmichael numbers
(miller-rabin-prime? 561)
(miller-rabin-prime? 1105)
(miller-rabin-prime? 1729)
(miller-rabin-prime? 2465)
(miller-rabin-prime? 2821)
(miller-rabin-prime? 6601)
