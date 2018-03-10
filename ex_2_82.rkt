#lang racket
(require "./ch_2_type_tag_helpers.rkt")
(require "ch_2_hash_ops.rkt")
(define (apply-generic op . args)
  (let* ((type-tags (map type-tag args))
         (proc (get op type-tags)))
    (if proc
        (apply proc (map contents args))
        (apply apply-generic (list op (successive-convert op args 0))))))

(define (maplist fnlist arglist)
  (if (or (null? fnlist) (null? arglist))
      null
      (cons ((car fnlist) (car arglist)) (maplist (cdr fnlist) (cdr arglist)))))

(define (successive-convert op args cur-pos)
  (define (no-proc-err)
    (error "APPLY-GENERIC-OP: could not find appropriate procedure" (list op args)))
  (if (>= cur-pos (length args))
      (no-proc-err) ; we've already tried all coercions
      (let* ((cur-type (type-tag (list-ref args cur-pos)))
             (new-types (map (lambda (x) cur-type) args))
             (proc (get op new-types)))
        (if proc
            (let ((conversion-fns (map (lambda (arg) (get-coercion (type-tag arg) cur-type)) args)))
              (if (memq #f conversion-fns)
                  (successive-convert op args (+ cur-pos 1)) ; at least one of the args cannot be converted into the type of the "current" arg
                  (maplist conversion-fns args)))
            ; converting all args into current arg's type is pointless, since there's no version of provided op for current arg's type
            (successive-convert op args (+ cur-pos 1))))))