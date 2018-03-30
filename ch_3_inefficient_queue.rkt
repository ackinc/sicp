#lang racket
(define (make-queue) (mcons 'queue null))
(define (empty-queue? q) (null? (mcdr q)))

(define (front-queue q)
  (if (empty-queue? q)
      (error "front-queue -- called with empty queue")
      (mcar (mcdr q))))

(define (last-pair x)
  (cond ((null? x) (error "last-pair -- called on empty list"))
        ((null? (mcdr x)) x)
        (else (last-pair (mcdr x)))))

(define (insert-queue! q item)
  (let ((lp (last-pair q)))
    (set-mcdr! lp (mcons item null))
    q))

(define (delete-queue! q)
  (if (empty-queue? q)
      (error "delete-queue! -- called on empty queue")
      (let ((item (front-queue q)))
        (set-mcdr! q (mcdr (mcdr q)))
        q)))

; TESTS
(define q (make-queue))
(empty-queue? q)
(insert-queue! q 2)
(insert-queue! q 3)
(eq? (front-queue q) 2)
(delete-queue! q)
(eq? (front-queue q) 3)
(delete-queue! q)
(empty-queue? q)