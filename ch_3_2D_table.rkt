#lang racket
(require (only-in "./ch_3_1D_table.rkt"
                  [insert! insert-1D!]
                  [assoc assoc-1D]
                  [lookup lookup-1D]))

(define (make-table) (mcons '*table* null))

(define (assoc key1 key2 records)
  (let ((subtable (assoc-1D key1 records)))
    (if subtable (assoc-1D key2 (mcdr subtable)) #f)))

(define (lookup key1 key2 table)
  (let ((record (assoc key1 key2 (mcdr table))))
    (if record (mcdr record) #f)))

(define (insert! key1 key2 value table)
  (let ((subtable (assoc-1D key1 (mcdr table))))
    (if subtable
        (let ((record (assoc-1D key2 (mcdr subtable))))
          (if record
              (set-mcdr! record value)
              (insert-1D! key2 value subtable)))
        (insert-1D! key1 (mcons (mcons key2 value) null) table))))

; TESTS
(define t (make-table))
(insert! 'letters 'a 1 t)
(insert! 'letters 'b 2 t)
(insert! 'math 'plus '+ t)
(insert! 'math 'minus '- t)
(insert! 'math 'times '* t)

(eq? (lookup 'letters 'a t) 1)
(eq? (lookup 'letters 'b t) 2)
(eq? (lookup 'letters 'c t) #f)
(eq? (lookup 'math 'plus t) '+)
(eq? (lookup 'math 'times t) '*)
(eq? (lookup 'math 'divide t) #f)

(insert! 'math 'plus '++ t)
(eq? (lookup 'math 'plus t) '++)