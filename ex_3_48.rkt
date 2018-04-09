#lang racket
(require "ch_3_incrementer.rkt")
(require (only-in "ch_3_concurrency_control.rkt"
                  make-serializer))

(define (accounts-manager)
  (let ((incrementer (incrementer)))
    (lambda (m)
      (if (eq? m 'new-account-number)
          (incrementer 'next!)
          (error "accounts-manager -- unrecognized message" m)))))

(define manager (accounts-manager))



(define (make-account balance)
  (define (withdraw amt)
    (if (< amt balance)
        (set! balance (- balance amt))
        (error "withdraw -- amt > balance" (list amt balance))))
  
  (define (deposit amt)
    (set! balance (+ balance amt)))

  (let ((serializer (make-serializer))
        (acc-number (manager 'new-account-number)))
    (define (dispatch m)
      (cond ((eq? m 'balance) balance)
            ((eq? m 'deposit) deposit)
            ((eq? m 'withdraw) withdraw)
            ((eq? m 'serializer) serializer)
            ((eq? m 'acc-number) acc-number)
            (else (error "dispatch -- unknown message" m))))
    dispatch))

(define (balance account) (account 'balance))

(define (withdraw account amount)
  (((account 'serializer) (account 'withdraw)) amount))

(define (deposit account amount)
  (((account 'serializer) (account 'deposit)) amount))

(define (exchange a1 a2)
  (let ((difference (- (balance a1) (balance a2))))
    ((a1 'withdraw) difference)
    ((a2 'deposit) difference)))

(define (serialized-exchange a1 a2)
  (let ((s1 (a1 'serializer))
        (s2 (a2 'serializer))
        (n1 (a1 'acc-number))
        (n2 (a2 'acc-number)))
    (if (< n1 n2)
        ((s1 (s2 exchange)) a1 a2)
        ((s2 (s1 exchange)) a1 a2))))

; TESTS
(define a1 (make-account 100))
(define a1num (a1 'acc-number))

(define a2 (make-account 30))
(define a2num (a2 'acc-number))

(serialized-exchange a1 a2)

(eq? (balance a1) 30)
(eq? (balance a2) 100)

(eq? (a1 'acc-number) a1num)
(eq? (a2 'acc-number) a2num)