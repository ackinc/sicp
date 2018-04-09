#lang racket
(require "queue.rkt")

(define q (make-queue))
(empty-queue? q)
(insert-queue! q 2)
(insert-queue! q 3)
(eq? (front-queue q) 2)
(delete-queue! q)
(eq? (front-queue q) 3)
(delete-queue! q)
(empty-queue? q)