#lang racket
(require "./ch_1_prime.rkt")
(require "./ch_1_timed_prime_test.rkt")

(define (test n) (timed-prime-test prime? n))

(define (search-for-primes a b)
  (cond ((even? a) (search-for-primes (+ a 1) b))
        ((> a b) (newline)
                 (display "END"))
        (else (test a)
              (search-for-primes (+ a 2) b))))

(search-for-primes 1000 1100)                           ; 0ms     1009              1013              1019
(search-for-primes 10000 10100)                         ; 0ms     10007             10009             10037
(search-for-primes 100000 100100)                       ; 0ms     100003            100019            100043
(search-for-primes 10000000 10000200)                   ; 0ms     10000019          10000079          10000103
(search-for-primes 100000000 100000200)                 ; 0ms     100000007         100000037         100000039
(search-for-primes 1000000000 1000000500)               ; 1ms     1000000007        1000000009        1000000021
(search-for-primes 10000000000 10000000100)             ; 3ms     10000000019       10000000033       10000000061
(search-for-primes 1000000000000 1000000000100)         ; 35ms    1000000000039     1000000000061     1000000000063
(search-for-primes 10000000000000 10000000000100)       ; 110ms   10000000000037    10000000000051    10000000000099
(search-for-primes 1000000000000000 1000000000000200)   ; 1100ms  1000000000000037  1000000000000091  1000000000000159
(search-for-primes 10000000000000000 10000000000000100) ; 3000ms  10000000000000061 10000000000000069 10000000000000079

; for use only with fermat-prime
;(search-for-primes 100000000000000000000000000000000 100000000000000000000000000001000)
;(search-for-primes 10000000000000000000000000000000000000000000000000000000000000000 10000000000000000000000000000000000000000000000000000000000001000)