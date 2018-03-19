#lang racket
(require "type_tag_helpers.rkt")
(require "hash_ops.rkt")
(require "../ch_2_list_operations.rkt")
(require "type_tower.rkt")

(provide apply-generic)

(define (equ? x y)
  (apply-generic 'equ? x y))

; ex 2.83
(define (raise x)
  ((get 'raise (type-tag x)) (contents x)))

; ex 2.84
(define (raise-n-times n)
  (lambda (x)
    (if (= n 0)
        x
        ((raise-n-times (- n 1)) (raise x)))))

; ex 2.85
(define (project x)
  ((get 'project (type-tag x)) (contents x)))

; ex 2.85
(define (drop x)
  (if (eq? (type-tag x) 'scheme-number)
      x
      (let* ((x-p (project x))
             (x-rp (raise x-p)))
        (if (equ? x x-rp) (drop x-p) x))))

; ex 2.84
(define (apply-generic op . args)
  (let* ((type-tags (map type-tag args))
         (proc (get op type-tags)))
      (if proc
          (let ((ans (apply proc (map contents args))))
            ans)
            ; ex 2.85
            ;(if (or (symbol? ans) (eq? #t ans) (eq? #f ans))
            ;    ans
            ;    (drop ans)))
          (let* ((type-levels (map type-level type-tags))
                 (highest-type-level (apply max type-levels))
                 (lowest-type-level (apply min type-levels)))
            (if (= highest-type-level lowest-type-level)
                (error "APPLY-GENERIC -- no proc found" (list op args))
                (let* ((levels-to-raise (map (lambda (level) (- highest-type-level level)) type-levels))
                       (raise-fns (map raise-n-times levels-to-raise))
                       (new-args (listmap raise-fns args)))
                  (apply apply-generic (list op new-args))))))))
