#lang racket
(require "for-each-except.rkt")
(provide make-connector has-value? get-value
         set-value! forget-value! connect!)

; helpers
(define (inform-about-new-value constraint) (constraint 'value-obtained))
(define (inform-about-lost-value constraint) (constraint 'value-lost))

(define (make-connector)
  (let ((value #f)
        (informant #f)
        (constraints null))
    
    (define (has-value?) (not (eq? value #f)))

    (define (get-value) value)
    
    (define (set-value! v setter)
      (if (not (has-value?))
          (begin (set! value v)
                 (set! informant setter)
                 (for-each-except setter constraints inform-about-new-value))
          (if (eq? value v)
              'ignore
              (error "set-value! -- connector already has value" (list value v setter)))))
    
    (define (forget-value! retractor)
      (if (eq? retractor informant)
          (begin (set! value #f)
                 (set! informant #f)
                 (for-each-except retractor constraints inform-about-lost-value))
          'ignore))
    
    (define (connect! constraint)
      (if (memq constraint constraints)
          'ok
          (begin (set! constraints (cons constraint constraints))
                 (if (has-value?)
                     (inform-about-new-value constraint)
                     'ok))))
    
    (define (dispatch m)
      (cond ((eq? m 'has-value?) (has-value?))
            ((eq? m 'get-value) (get-value))
            ((eq? m 'set-value!) set-value!)
            ((eq? m 'forget-value!) forget-value!)
            ((eq? m 'connect!) connect!)
            (else (error "connector dispatch -- invalid message" m))))
    dispatch))

(define (has-value? connector) (connector 'has-value?))
(define (get-value connector) (connector 'get-value))
(define (set-value! connector value informant) ((connector 'set-value!) value informant))
(define (forget-value! connector retractor) ((connector 'forget-value!) retractor))
(define (connect! connector constraint) ((connector 'connect!) constraint))