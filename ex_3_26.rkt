#lang racket
(define (make-tree entry) (mcons 'tree (mcons entry (mcons null null))))
(define (entry tree) (mcar (mcdr tree)))
(define (left tree) (mcar (mcdr (mcdr tree))))
(define (right tree) (mcdr (mcdr (mcdr tree))))

(define (tree? x) (and (mpair? x) (eq? (mcar x) 'tree)))
(define (set-entry! tree x) (set-mcar! (mcdr tree) x))
(define (set-left-subtree! tree x) (set-mcar! (mcdr (mcdr tree)) (make-tree x)))
(define (set-right-subtree! tree x) (set-mcdr! (mcdr (mcdr tree)) (make-tree x)))

(define (make-record key val) (mcons key val))
(define (key-record record) (mcar record))
(define (val-record record) (mcdr record))
(define (set-val-record! record x) (set-mcdr! record x))

(define (make-table)
  (let ((table (mcons 'table null)))

    (define (empty?) (null? (mcdr table)))

    (define (assoc key tree)
      (cond ((null? tree) #f)
            (else (let* ((record (entry tree))
                         (cur-key (key-record record)))
                    (cond ((eq? cur-key key) record)
                          ((symbol<? cur-key key) (assoc key (right tree)))
                          (else (assoc key (left tree))))))))

    (define (lookup keys)
      (define (helper remaining-keys tree-or-value)
        (cond ((null? remaining-keys) tree-or-value)
              ((not (tree? tree-or-value)) #f)
              (else (let ((record (assoc (car remaining-keys) tree-or-value)))
                      (if record
                          (helper (cdr remaining-keys) (val-record record))
                          #f)))))
      (helper keys (mcdr table)))

    (define (insert! key value)
      (define (helper tree)
        (let* ((record (entry tree))
               (cur-key (key-record record)))
          (cond ((eq? cur-key key) (set-val-record! record value))
                ((symbol<? cur-key key) (let ((rt (right tree)))
                                          (if (null? rt)
                                              (set-right-subtree! tree (make-tree (make-record key value)))
                                              (helper rt))))
                (else (let ((lt (left tree)))
                        (if (null? lt)
                            (set-left-subtree! tree (make-tree (make-record key value)))
                            (helper lt)))))))
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
(eq? (lookup '(name) t) #f)
(eq? (lookup '(firstname) t) "Anirudh")