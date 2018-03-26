#lang racket
(define (call-the-cops) (display "The cops have been alerted.")(newline))

(define (make-account balance password)
  (define incorrect-password-attempts 0)

  (define (withdraw amount)
    (if (<= amount balance)
        (begin (set! balance (- balance amount))
               balance)
        "Insufficient funds"))

  (define (deposit amount)
    (set! balance (+ balance amount))
    balance)
  
  (define (inc-ipa) (set! incorrect-password-attempts (+ incorrect-password-attempts 1)))
  (define (reset-ipa) (set! incorrect-password-attempts 0))
  (define (incorrect-password-handler x)
    (if (>= incorrect-password-attempts 7)
        (call-the-cops)
        (inc-ipa))
    "Incorrect password")

  (define (dispatch password)
    (lambda (pwd m)
      (if (not (eq? pwd password))
          incorrect-password-handler
          (begin (reset-ipa)
                 (cond ((eq? m 'withdraw) withdraw)
                       ((eq? m 'deposit) deposit)
                       ((eq? m 'make-joint) (lambda (new-pass) (dispatch new-pass)))
                       (else (lambda (x) "Invalid message")))))))
  (dispatch password))

(define (make-joint acc pwd new-pwd)
  ((acc pwd 'make-joint) new-pwd))

; TESTS
(display "Basic tests")(newline)
(define anirudh-acc (make-account 200 'anirudh))
(eq? ((anirudh-acc 'a 'withdrawal) 50) "Incorrect password")
(eq? ((anirudh-acc 'anirudh 'withdrawal) 50) "Invalid message")
(eq? ((anirudh-acc 'anirudh 'withdraw) 500) "Insufficient funds")
(eq? ((anirudh-acc 'anirudh 'withdraw) 50) 150)
(eq? ((anirudh-acc 'anirudh 'deposit) 50) 200)

(display "7 incorrect attempts in a row")(newline)
(eq? ((anirudh-acc 'a 'withdrawal) 50) "Incorrect password")
(eq? ((anirudh-acc 'a 'withdrawal) 50) "Incorrect password")
(eq? ((anirudh-acc 'a 'withdrawal) 50) "Incorrect password")
(eq? ((anirudh-acc 'a 'withdrawal) 50) "Incorrect password")
(eq? ((anirudh-acc 'a 'withdrawal) 50) "Incorrect password")
(eq? ((anirudh-acc 'a 'withdrawal) 50) "Incorrect password")
(eq? ((anirudh-acc 'a 'withdrawal) 50) "Incorrect password")

(display "This next attempt will bring the cops")(newline)
(eq? ((anirudh-acc 'a 'withdrawal) 50) "Incorrect password")

(display "This will reset incorrect-password-attempts to 0")(newline)
(eq? ((anirudh-acc 'anirudh 'deposit) 50) 250)

(display "Testing make-joint")(newline)
(define ani-acc (make-joint anirudh-acc 'anirudh 'ani))
(eq? ((ani-acc 'ani 'deposit) 150) 400)
(eq? ((ani-acc 'ani 'withdraw) 5) 395)
(eq? ((anirudh-acc 'anirudh 'deposit) 50) 445)
(eq? ((anirudh-acc 'anirudh 'withdraw) 5) 440)

(display "Accessing ani-acc with the password of anirudh-acc, and vice-versa, is not allowed")(newline)
(eq? ((anirudh-acc 'ani 'withdrawal) 50) "Incorrect password")
(eq? ((ani-acc 'anirudh 'withdrawal) 50) "Incorrect password")