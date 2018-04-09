#lang racket
(require "wires.rkt")
(require "basic_gates.rkt")
(provide half-adder full-adder ripple-carry-adder ripple-carry-adder-alt)

(define (half-adder a b s c)
  (let ((d (make-wire))
        (not-c (make-wire)))
    (and-gate a b c)
    (inverter c not-c)
    (or-gate a b d)
    (and-gate d not-c s)
    'ok))

(define (full-adder a b c-in s c-out)
  (let ((c-inter1 (make-wire))
        (c-inter2 (make-wire)))
    (half-adder b c-in s1 c-inter1)
    (half-adder s1 a s c-inter2)
    (or-gate c-inter1 c-inter2 c-out)
    'ok))

(define (reverse L)
  (define (iter rem-L acc)
    (if (null? rem-L)
        acc
        (iter (cdr rem-L) (cons (car rem-L) acc))))
  (iter L null))

; ex 3.30
(define (ripple-carry-adder A B S C)
  (cond ((not (= (length A) (length B) (length S))) (error "ripple-carry-adder -- A, B, and S must have same length"))
        ((null? A) (error "ripple-carry-adder -- A, B, and S cannot be empty lists"))
        (else (let ((A-rev (reverse A))
                    (B-rev (reverse B))
                    (S-rev (reverse S)))
                (define (helper Ak Bk C-in Sk)
                  (if (null? (cdr Ak))
                      (full-adder (car Ak) (car Bk) C-in (car Sk) C)
                      (let ((Cnext (make-wire)))
                        (full-adder (car Ak) (car Bk) C-in (car Sk) Cnext)
                        (helper (cdr Ak) (cdr Bk) Cnext (cdr Sk)))))
                (helper A-rev B-rev (make-wire) S-rev)))))

; ex 3.30
; a more elegant alternate implementation
(define (ripple-carry-adder-alt A B S C)
  (if (null? A)
      'ok
      (let ((Cprev (make-wire)))
        (ripple-carry-adder-alt (cdr A) (cdr B) (cdr S) Cprev)
        (full-adder (car A) (car B) Cprev (car S) C))))