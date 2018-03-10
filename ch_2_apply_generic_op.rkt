#lang racket
(require "ch_2_type_tag_helpers.rkt")
(require "ch_2_hash_ops.rkt")

(provide apply-generic-op)

(define (apply-generic-op op . args)
  (let ((type-tags (map type-tag args)))
    (let ((proc (get op type-tags)))
      (if proc
          (apply proc (map contents args))
          (error "APPLY-GENERIC-OP: could not find key in table" op args)))))