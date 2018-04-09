#lang racket
(require "connector.rkt")
(require "constraints.rkt")

(define (celsius-fahrenheit-converter c f)
  (let ((five (make-connector))
        (nine (make-connector))
        (thirty-two (make-connector))
        (nine-times-c (make-connector))
        (f-minus-thirty-two (make-connector)))
    (constant 5 five)
    (constant 9 nine)
    (constant 32 thirty-two)

    (multiplier c nine nine-times-c)

    (multiplier five f-minus-thirty-two nine-times-c)
    (adder thirty-two f-minus-thirty-two f)))

(define c (make-connector))
(define f (make-connector))

(celsius-fahrenheit-converter c f)

(probe 'C c)
(probe 'F f)

(set-value! f 212 'user)

(forget-value! f 'user)

(set-value! c 0 'user)