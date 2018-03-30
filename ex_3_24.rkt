#lang racket
(define (make-table same-key?)
  (let ((table (mcons 'table null)))

    (define (assoc key records)
      (cond ((null? records) #f)
            ((same-key? key (mcar (mcar records))) (mcar records))
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
(define t (make-table equal?))
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

(define (approx-equal x y)
  (if (and (number? x) (number? y))
      (< (abs (- x y)) 0.5)
      (equal? x y)))
(define t2 (make-table approx-equal))
(insert! 'numbers 1 'one t2)
(insert! 'numbers 8.9 'eightpointnine t2)
(insert! 'numbers 30 'thirty t2)
(insert! 'numbers 3 'three t2)
(insert! 'numbers 0 'zero t2)
(eq? (lookup 'numbers 1 t2) 'one)
(eq? (lookup 'numbers 9 t2) 'eightpointnine)
(eq? (lookup 'numbers 9.4 t2) #f)
(eq? (lookup 'numbers 30 t2) 'thirty)
(eq? (lookup 'numbers 30.3 t2) 'thirty)
(eq? (lookup 'numbers 30.6 t2) #f)
