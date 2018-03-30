#lang racket
(define (make-table)
  (let ((table (mcons 'table null)))

    (define (assoc key records)
      (cond ((null? records) #f)
            ((equal? key (mcar (mcar records))) (mcar records))
            (else (assoc key (mcdr records)))))

    (define (insert! key1 key2 value)
      (let ((subtable (assoc key1 (mcdr table))))
        (if subtable
            (let ((record (assoc key2 (mcdr subtable))))
              (if record
                  (set-mcdr! record value)
                  (set-mcdr! subtable (mcons (mcons key2 value) (mcdr subtable)))))
            (begin (set-mcdr! table (mcons (mcons key1 null) (mcdr table)))
                   (insert! key1 key2 value)))))

    (define (lookup key1 key2)
      (let ((subtable (assoc key1 (mcdr table))))
        (if subtable
            (let ((record (assoc key2 (mcdr subtable))))
              (if record
                  (mcdr record)
                  #f))
            #f)))

    (define (dispatch m)
      (cond ((eq? m 'insert) insert!)
            ((eq? m 'lookup) lookup)
            (else (error "dispatch -- invalid message" m))))
    dispatch))

(define (insert! key1 key2 val table) ((table 'insert) key1 key2 val))
(define (lookup key1 key2 table) ((table 'lookup) key1 key2))

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

(define t2 (make-table))
(insert! 'letters 'c 3 t2)
(insert! 'letters 'd 4 t2)
(eq? (lookup 'letters 'a t2) #f)
(eq? (lookup 'letters 'c t2) 3)
(eq? (lookup 'letters 'd t2) 4)