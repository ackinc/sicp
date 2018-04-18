#lang racket
(require "stream_procs.rkt")
(provide ones integers)
(define ones (cons-stream 1 ones))
(define integers (cons-stream 1 (add-streams ones integers)))