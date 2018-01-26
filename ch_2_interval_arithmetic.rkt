#lang racket
(provide make-interval upper-bound lower-bound
         make-interval-cw center-interval width-interval
         make-interval-cp tolerance-interval
         add-interval sub-interval mul-interval mul-interval2 div-interval
         print-interval)

(define (make-interval a b)
  (if (< a b) (cons a b) (cons b a)))
(define (lower-bound i) (car i))
(define (upper-bound i) (cdr i))

(define (make-interval-cw center width)
  (make-interval (- center width) (+ center width)))
(define (center-interval i)
  (/ (+ (lower-bound i) (upper-bound i)) 2))
(define (width-interval i)
  (/ (- (upper-bound i) (lower-bound i)) 2))

(define (make-interval-cp center tolerance)
  (let ((width (* center (/ tolerance 100))))
    (make-interval-cw center width)))
(define (tolerance-interval i)
  (* (/ (width-interval i) (center-interval i)) 100))

(define (add-interval i1 i2)
  (make-interval (+ (lower-bound i1) (lower-bound i2))
                 (+ (upper-bound i1) (upper-bound i2))))

(define (sub-interval i1 i2)
  (add-interval i1 (make-interval (- (lower-bound i2)) (- (upper-bound i2)))))

(define (mul-interval i1 i2)
  (let ((l1 (lower-bound i1))
        (l2 (lower-bound i2))
        (u1 (upper-bound i1))
        (u2 (upper-bound i2)))
    (make-interval (min (* l1 l2) (* l1 u2) (* u1 l2) (* u1 u2))
                   (max (* l1 l2) (* l1 u2) (* u1 l2) (* u1 u2)))))

; ex 2.11
(define (mul-interval2 i1 i2)
  (let ((l1 (lower-bound i1))
        (u1 (upper-bound i1))
        (l2 (lower-bound i2))
        (u2 (upper-bound i2)))
    (cond ((and (>= l1 0) (>= u1 0) (>= l2 0) (>= u2 0)) (make-interval (* l1 l2) (* u1 u2)))
          ((and (>= l1 0) (>= u1 0) (< l2 0) (>= u2 0)) (make-interval (* u1 l2) (* u1 u2)))
          ((and (>= l1 0) (>= u1 0) (< l2 0) (< u2 0)) (make-interval (* u1 l1) (* l1 u2)))
          ((and (< l1 0) (>= u1 0) (>= l2 0) (>= u2 0)) (make-interval (* l1 u2) (* u1 u2)))
          ((and (< l1 0) (>= u1 0) (< l2 0) (< u2 0)) (make-interval (* u1 l2) (* l1 u2)))
          ((and (< l1 0) (< u1 0) (>= l2 0) (>= u2 0)) (make-interval (* l1 u2) (* u1 l2)))
          ((and (< l1 0) (< u1 0) (< l2 0) (>= u2 0)) (make-interval (* l1 u2) (* l1 l2)))
          ((and (< l1 0) (< u1 0) (< l2 0) (< u2 0)) (make-interval (* u1 u2) (* l1 l2)))
          ((and (< l1 0) (>= u1 0) (< l2 0) (>= u2 0)) (make-interval (min (* l1 u2) (* l2 u1)) (max (* l1 l2) (* u1 u2))))
          (else #f))))

(define (div-interval i1 i2)
  (if (and (<= (lower-bound i2) 0) (>= (upper-bound i2) 0))
      (error "Error: dividing by an interval that spans 0")
      (mul-interval i1 (make-interval (/ 1 (lower-bound i2))
                                      (/ 1 (upper-bound i2))))))

(define (print-interval i)
  (display "[")
  (display (lower-bound i))
  (display ", ")
  (display (upper-bound i))
  (display "]"))