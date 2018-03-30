#lang racket
(provide make-table assoc lookup insert!)

(define (make-table) (mcons '*table* null))

(define (assoc key records)
  (cond ((null? records) #f)
        ((equal? (mcar (mcar records)) key) (mcar records))
        (else (assoc key (mcdr records)))))

(define (lookup key table)
  (let ((record (assoc key (mcdr table))))
    (if record (mcdr record) #f)))

(define (insert! key value table)
  (let ((record (assoc key (mcdr table))))
    (if record
        (set-mcdr! record value)
        (set-mcdr! table (mcons (mcons key value) (mcdr table))))))

; TESTS
(define t (make-table))
(insert! 'a 1 t)
(insert! 'b 2 t)
(eq? (lookup 'a t) 1)
(eq? (lookup 'b t) 2)
(eq? (lookup 'c t) #f)
(insert! 'a 3 t)
(eq? (lookup 'a t) 3)