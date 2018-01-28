#lang racket
(require (only-in "./ch_2_sequence_ops.rkt"
                  filter
                  flatmap
                  [enumerate-interval enumerate-sequence]))
(require (only-in "./ch_1_prime.rkt"
                  [miller-rabin-prime? prime?]))

(provide unique-pairs)

(define (unique-pairs n)
  (flatmap (lambda (i)
             (map (lambda (j) (list i j))
                  (enumerate-sequence 1 (- i 1))))
           (enumerate-sequence 1 n)))

(define (prime-sum-pairs n)
  (filter (lambda (x) (prime? (caddr x)))
          (map (lambda (pair) (list (car pair) (cadr pair) (+ (car pair) (cadr pair))))
               (unique-pairs n))))