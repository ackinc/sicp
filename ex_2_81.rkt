#lang racket
(require "ch_2_type_tag_helpers.rkt")
(require "ch_2_hash_ops.rkt")
(define (apply-generic-op op . args)
  (let ((type-tags (map type-tag args)))
    (let ((proc (get op type-tags)))
      (if proc
          (apply proc args)
          (if (= (length args) 2)
              (let ((t1 (car type-tags))
                    (t2 (cadr type-tags))
                    (a1 (car args))
                    (a2 (cadr args)))
                (if (eq? t1 t2)
                    (error "APPLY-GENERIC-OP: could not find key in table" op args)
                    (let ((t1->t2 (get-coercion t1 t2))
                          (t2->t1 (get-coercion t2 t1)))
                      (cond (t1->t2 (apply-generic-op op (t1->t2 a1) a2))
                            (t2->t1 (apply-generic-op op a1 (t2->t1 a2)))
                            (else (error "APPLY-GENERIC-OP: could not find key in table" op args))))))
              (error "APPLY-GENERIC-OP: could not find key in table" op args))))))