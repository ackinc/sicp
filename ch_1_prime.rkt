#lang racket
(#%require (lib "27.ss" "srfi"))

(provide prime? smallest-divisor faster-prime? fermat-prime? slower-fermat-prime? miller-rabin-prime?)

(define (square x) (* x x))

;;; generic implementation
(define (next k) (+ k 1))

(define (smallest-divisor n)
  (define (try k)
    (cond ((> (square k) n) n)
          ((= (remainder n k) 0) k)
          (else (try (next k)))))
  (try 2))

(define (prime? n)
  (= (smallest-divisor n) n))

;;; skipping even numbers
(define (faster-next k) (if (= k 2) 3 (+ k 2)))

(define (faster-smallest-divisor n)
  (define (try k)
    (cond ((> (square k) n) n)
          ((= (remainder n k) 0) k)
          (else (try (faster-next k)))))
  (try 2))

(define (faster-prime? n)
  (= (faster-smallest-divisor n) n))

;;; using fermat's little theorem
(define (expmod base exp m)
  (cond ((= exp 0) 1)
        ((even? exp) (remainder (square (expmod base (/ exp 2) m)) m))
        (else (remainder (* base (expmod base (- exp 1) m)) m))))

(define (fermat-test-once n)
  (define a (+ 1 (random-integer (- n 1))))
  (= (expmod a n n) a))

(define (fermat-prime? n)
  (define (repeat times)
    (cond ((= 0 times) #t)
          ((not (fermat-test-once n)) #f)
          (else (repeat (- times 1)))))
  (repeat 100))

;;; slower fermat-prime, using fast-exp instead of expmod
(define (fast-exp b n)
  (cond ((= n 0) 1)
        ((even? n) (fast-exp (square b) (/ n 2)))
        (else (* b (fast-exp b (- n 1))))))

(define (slower-fermat-test-once n)
  (define a (+ 1 (random-integer (- n 1))))
  (= (remainder (fast-exp a n) n) a))

(define (slower-fermat-prime? n)
  (define (repeat times)
    (cond ((= 0 times) #t)
          ((not (slower-fermat-test-once n)) #f)
          (else (repeat (- times 1)))))
  (repeat 100))

;;; Miller-Rabin test (cannot be fooled, unlike Fermat's test)
(define (ntsr? candidate n)
  (and (not (= candidate 1))
       (not (= candidate (- n 1)))
       (= (remainder (square candidate) n) 1)))

(define (expmod-mr base exp m)
  (cond ((= exp 0) 1)
        ((even? exp) (define ntsr-candidate (expmod base (/ exp 2) m))
                     (if (ntsr? ntsr-candidate m) 0 (remainder (square ntsr-candidate) m)))
        (else (remainder (* base (expmod base (- exp 1) m)) m))))

(define (miller-rabin-test-once n)
  (define a (+ 1 (random-integer (- n 1))))
  (= (expmod-mr a (- n 1) n) 1))

(define (miller-rabin-prime? n)
  (define (repeat times)
    (cond ((= 0 times) #t)
          ((not (miller-rabin-test-once n)) #f)
          (else (repeat (- times 1)))))
  (repeat 100))