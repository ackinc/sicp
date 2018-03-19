#lang racket
(require "../ch_2_list_operations.rkt")
(require "type_tag_helpers.rkt")

(provide type-level is-lowest-type)

(define tower '(scheme-number rational complex polynomial))

(define (type-level type)
  (let ((level (list-search type tower)))
    (or level (error "TYPE-LEVEL -- unknown type" type))))

(define (is-lowest-type x)
  (= (type-level (type-tag x)) 0))