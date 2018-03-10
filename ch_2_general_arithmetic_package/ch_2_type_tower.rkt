#lang racket
(require "../ch_2_list_operations.rkt")

(provide type-level)

(define tower '(scheme-number rational complex))

(define (type-level type)
  (let ((level (list-search type tower)))
    (or level (error "TYPE-LEVEL -- unknown type" type))))