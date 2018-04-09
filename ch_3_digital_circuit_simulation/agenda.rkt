#lang racket
(require "queue.rkt")
(provide make-agenda empty-agenda?
         first-agenda-item add-to-agenda! remove-first-agenda-item!
         current-time)

(define (make-time-segment time queue) (cons time queue))
(define (segment-time segment) (car segment))
(define (segment-queue segment) (cdr segment))

(define (make-agenda) (mcons 0 null))

(define (current-time agenda) (mcar agenda))
(define (set-current-time! agenda time) (set-mcar! agenda time))

(define (segments agenda) (mcdr agenda))
(define (set-segments! agenda segments) (set-mcdr! agenda segments))

(define (first-segment agenda) (mcar (segments agenda)))
(define (rest-segments agenda) (mcdr (segments agenda)))

(define (empty-agenda? agenda) (null? (segments agenda)))

(define (add-to-agenda! time action agenda)
  (define (belongs-before? segments)
    (or (null? segments)
        (< time (segment-time (mcar segments)))))
  
  (define (make-new-time-segment time action)
    (let ((q (make-queue)))
      (insert-queue! q action)
      (make-time-segment time q)))

  (define (add-to-segments! segments)
    (if (= time (segment-time (mcar segments)))
        (insert-queue! (segment-queue (mcar segments)) action)
        (let ((rest-segments (mcdr segments)))
          (if (belongs-before? rest-segments)
              (set-mcdr! segments (mcons (make-new-time-segment time action) rest-segments))
              (add-to-segments! rest-segments)))))
  
  (let ((segments (segments agenda)))
    (if (belongs-before? segments)
        (set-segments! agenda (mcons (make-new-time-segment time action) segments))
        (add-to-segments! segments))))

(define (remove-first-agenda-item! agenda)
  (if (empty-agenda? agenda)
      (error "remove-first-agenda-item! -- called on empty agenda")
      (let* ((fs (first-segment agenda))
             (action-queue (segment-queue fs)))
        (delete-queue! action-queue)
        (if (empty-queue? action-queue)
            (set-segments! agenda (rest-segments agenda))
            'done))))

(define (first-agenda-item agenda)
  (if (empty-agenda? agenda)
      (error "first-agenda-item-agenda -- called on empty agenda")
      (let ((fs (first-segment agenda)))
        (set-current-time! agenda (segment-time fs))
        (front-queue (segment-queue fs)))))