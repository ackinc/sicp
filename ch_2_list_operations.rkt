#lang racket
(provide list-ref length append map list-search listmap list-equal?)

(define (list-ref items n)
  (if (= n 0)
      (car items)
      (list-ref (cdr items) (- n 1))))

(define (length items)
  (if (null? items)
      0
      (+ 1 (length (cdr items)))))

(define (append l1 l2)
  (if (null? l1)
      l2
      (cons (car l1) (append (cdr l1) l2))))

(define (map proc items)
  (if (null? items)
      '()
      (cons (proc (car items))
            (map proc (cdr items)))))

(define (count-leaves items)
  (cond ((null? items) 0)
        ((not (pair? items)) 1)
        (else (+ (count-leaves (car items))
                 (count-leaves (cdr items))))))

(define (reverse items)
  (define (helper items-left ans)
    (if (null? items-left)
        ans
        (helper (cdr items-left) (cons (car items-left) ans))))
  (helper items null))

; ex 2.27
(define (deep-reverse items)
  (define (helper items-left ans)
    (cond ((null? items-left) ans)
          ((not (pair? (car items-left))) (helper (cdr items-left) (cons (car items-left) ans)))
          (else (helper (cdr items-left) (cons (deep-reverse (car items-left)) ans)))))
  (helper items null))

; ex 2.28
(define (fringe items)
  (cond ((null? items) null)
        ((not (pair? (car items))) (cons (car items) (fringe (cdr items))))
        (else (append (fringe (car items)) (fringe (cdr items))))))

; the methods below are used in apply-generic-op (ex 2.84 and later)
(define (list-search item items)
  (let ((tmp (memq item items)))
    (if tmp (- (length items) (length tmp)) #f)))

(define (listmap fnlist arglist)
  (if (or (null? fnlist) (null? arglist))
      null
      (cons ((car fnlist) (car arglist)) (listmap (cdr fnlist) (cdr arglist)))))

(define (list-equal? l1 l2)
  (cond ((and (null? l1) (null? l2)) #t)
        ((or (null? l1) (null? l2)) #f)
        (else (let ((f1 (car l1))
                    (f2 (car l2))
                    (r1 (cdr l1))
                    (r2 (cdr l2)))
                (cond ((and (list? f1) (list? f2)) (and (list-equal? f1 f2) (list-equal? r1 r2)))
                      ((or (list? f1) (list? f2)) #f)
                      ((and (pair? f1) (pair? f2)) (and (equal? (car f1) (car f2)) (equal? (cdr f1) (cdr f2)) (list-equal? r1 r2)))
                      ((or (pair? f1) (pair? f2)) #f)
                      (else (and (equal? f1 f2) (list-equal? r1 r2))))))))