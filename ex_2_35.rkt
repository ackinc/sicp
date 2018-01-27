#lang racket
(require "./ch_2_sequence_ops.rkt")
(define (count-leaves tree)
  (accumulate-alt (lambda (subtree ans)
                (cond ((null? subtree) ans)
                      ((not (pair? subtree)) (+ ans 1))
                      (else (+ ans (count-leaves subtree)))))
              0
              tree))

; TESTS
(count-leaves '()) ; 0
(count-leaves (list 1)) ; 1
(count-leaves (list 1 2 3 4 5)) ; 5
(count-leaves (list 1 2 3 (list 4 5 6) 7 8 9 (list 10))) ; 10
(count-leaves (list 1 2 3 (list 4 (list 5 5.5 5.75 5.875) 6) 7 8 9 (list 10))) ; 13