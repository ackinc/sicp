#lang racket
(define (make-point x y) (cons x y))
(define (x-point point) (car point))
(define (y-point point) (cdr point))

(define (make-segment start-point end-point)
  (cond ((< (x-point start-point) (x-point end-point)) (cons start-point end-point))
        ((> (x-point start-point) (x-point end-point)) (cons end-point start-point))
        ((< (y-point start-point) (y-point end-point)) (cons start-point end-point))
        (else (cons end-point start-point))))
(define (start-segment segment) (car segment))
(define (end-segment segment) (cdr segment))

(define (euclid-dist p1 p2)
  (define (square x) (* x x))
  (let ((x1 (x-point p1))
        (x2 (x-point p2))
        (y1 (y-point p1))
        (y2 (y-point p2)))
    (sqrt (+ (square (- x2 x1)) (square (- y2 y1))))))

; implementation 1
(define (make-rectangle a b c d) (cons (cons a b) (cons c d)))
(define (vertex-1 rect) (car (car rect)))
(define (vertex-2 rect) (cdr (car rect)))
(define (vertex-3 rect) (car (cdr rect)))
(define (vertex-4 rect) (cdr (cdr rect)))

(define (length rect) (euclid-dist (vertex-1 rect) (vertex-2 rect)))
(define (breadth rect) (euclid-dist (vertex-2 rect) (vertex-3 rect)))

; implementation 2
;(define (make-rectangle side1 side2) (cons side1 side2))
;(define (side-1 rect) (car rect))
;(define (side-2 rect) (cdr rect))

;(define (length rect)
;  (let ((s (side-1 rect)))
;    (euclid-dist (start-segment s) (end-segment s))))
;(define (breadth rect)
;  (let ((s (side-2 rect)))
;    (euclid-dist (start-segment s) (end-segment s))))

; general procedures
(define (perimeter rect) (+ (* 2 (length rect)) (* 2 (breadth rect))))
(define (area rect) (* (length rect) (breadth rect)))

; TESTS
; impl 1
(define rect (make-rectangle (make-point 0 0) (make-point 4 0) (make-point 4 2) (make-point 0 2)))
; impl 2
;(define rect (make-rectangle (make-segment (make-point 0 0) (make-point 4 0)) (make-segment (make-point 4 0) (make-point 4 2))))

(= (length rect) 4)
(= (breadth rect) 2)
(= (area rect) 8)
(= (perimeter rect) 12)