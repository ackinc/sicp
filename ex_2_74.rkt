#lang racket
(require "./ch_2_type_tags.rkt")

;div1 file
;(('Anirudh ('ADDRESS 'Jubilee) ('SALARY 138000))
; ('Anirud ('ADDRESS 'Pleasant) ('SALARY 131000))
; ('Aniru ('ADDRESS 'Hills) ('SALARY 135000)))
(define (install-div1-package)
  (define (get-name employee) (car employee))
  (define (get-salary employee) (cadr (caddr employee)))
  (define (find-employee name records)
    (cond ((null? records) #f)
          ((eq? (get-name (car records)) name) (attach-tag 'div1 (car records)))
          (else (find-employee name (cdr records)))))

  (put 'div1 'find-employee find-employee)
  (put 'div1 'get-salary get-salary)
  'done)

;div2 file
;(('Anak ('SALARY 138050) ('ADDRESS 'Trantor))
; ('Anaki ('SALARY 138300) ('ADDRESS 'Coruscant))
; ('Anakin ('SALARY 158000) ('ADDRESS 'Naboo)))
(define (install-div2-package)
  (define (get-name employee) (car employee))
  (define (get-salary employee) (cadr (cadr employee)))
  (define (find-employee name records)
    (cond ((null? records) #f)
          ((eq? (get-name (car records)) name) (attach-tag 'div2 (car records)))
          (else (find-employee name (cdr records)))))

  (put 'div2 'find-employee find-employee)
  (put 'div2 'get-salary get-salary)
  'done)

;a
(define (get-record name tagged-file)
  ((get (type-tag tagged-file) 'find-employee) name (contents tagged-file)))

;b
;assumed that we get tagged-record using get-record above
(define (get-salary tagged-record)
  ((get (type-tag tagged-record) 'get-salary) (contents tagged-record)))

;c
(define (find-employee-record name files-list)
  (cond ((null? files-list) #f)
        (else (or (get-record name (car files-list))
                  (find-employee-record name (cdr files-list))))))