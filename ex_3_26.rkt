#lang racket
(define (make-tree entry) (mcons entry (mcons null null)))
(define (entry tree) (mcar tree))
(define (left tree) (mcar (mcdr tree)))
(define (right tree) (mcdr (mcdr tree)))
(define (set-left-subtree! tree x) (set-mcar! (mcdr tree) (make-tree x)))
(define (set-right-subtree! tree x) (set-mcdr! (mcdr tree) (make-tree x)))

(define (make-record key val) (mcons key val))
(define (key-record record) (mcar record))
(define (val-record record) (mcdr record))
(define (set-val-record! record x) (set-mcdr! record x))

(define (make-table)
  (let ((table (mcons 'table null)))

    (define (empty?) (null? (mcdr table)))

    (define (assoc key)
      (define (helper tree)
        (cond ((null? tree) #f)
              (else (let* ((record (entry tree))
                           (cur-key (key-record record)))
                      (cond ((eq? cur-key key) record)
                            ((symbol<? cur-key key) (helper (right tree)))
                            (else (helper (left tree))))))))
      (helper (mcdr table)))

    (define (lookup key)
      (let ((record (assoc key)))
        (if record (val-record record) #f)))

    (define (insert! key value)
      (define (helper tree)
        (let* ((record (entry tree))
               (cur-key (key-record record)))
          (cond ((eq? cur-key key) (set-val-record! record value))
                ((symbol<? cur-key key) (if (null? (right tree))
                                            (set-right-subtree! tree (make-record key value))
                                            (helper (right tree))))
                (else (if (null? (left tree))
                          (set-left-subtree! tree (make-record key value))
                          (helper (left tree)))))))
      (if (empty?)
          (set-mcdr! table (make-tree (make-record key value)))
          (helper (mcdr table))))

    (define (dispatch m)
      (cond ((eq? m 'empty?) (empty?))
            ((eq? m 'insert!) insert!)
            ((eq? m 'lookup) lookup)
            (else (error "dispatch -- invalid message" m))))

    dispatch))

(define (insert! key val tree) ((tree 'insert!) key val))
(define (lookup key tree) ((tree 'lookup) key))


; TESTS
(define t (make-table))

(eq? (t 'empty?) #t)

(display "single-key tests")(newline)
(insert! 'firstname "Anirudh" t)
(insert! 'lastname "Nimmagadda" t)
(insert! 'age 26 t)
(insert! 'sex "Male" t)

(eq? (t 'empty?) #f)
(eq? (lookup 'name t) #f)
(eq? (lookup 'firstname t) "Anirudh")