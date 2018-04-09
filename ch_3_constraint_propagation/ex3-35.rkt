#lang racket
(require "connector.rkt")
(require "constraints.rkt")

(define a (make-connector))
(define sqa (make-connector))

(squarer a sqa)

(probe 'a a)
(probe 'sqa sqa)

(set-value! a 2 'user)

(forget-value! a 'user)

(set-value! sqa 16 'user)