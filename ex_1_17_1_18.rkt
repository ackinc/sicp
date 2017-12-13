#lang racket
(define (mult a b)
  (if (= b 0) 0 (+ a (mult a (- b 1)))))

(define (mult-iter a b)
  (define (helper counter acc)
    (if (= counter b) acc (helper (+ counter 1) (+ a acc))))
  (helper 0 0))

(define (double x) (+ x x))

(define (halve x) (/ x 2))

(define (fast-mult a b)
  (cond ((= b 0) 0)
        ((even? b) (fast-mult (double a) (halve b)))
        (else (+ a (fast-mult a (- b 1))))))

(define (fast-mult-iter a b)
  (define (helper a b acc)
    (cond ((= b 0) acc)
          ((even? b) (helper (double a) (halve b) acc))
          (else (helper a (- b 1) (+ acc a)))))
  (helper a b 0))