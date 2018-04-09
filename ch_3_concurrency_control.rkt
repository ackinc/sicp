#lang racket
(provide make-serializer make-mutex)

(define (make-serializer)
  (let ((mutex (make-mutex)))
    (lambda (p)
      (define (serialized-p . args)
        (mutex 'acquire)
        (let ((val (apply p args)))
          (mutex 'release)
          val))
      serialized-p)))

(define (make-mutex)
  (let ((cell (mcons #f null)))
    (define (test-and-set!)
      (if (mcar cell)
          #t
          (begin (set-mcar! cell #t)
                 #f)))
    (define (clear!)
      (set-mcar! cell #f))
    (define (the-mutex m)
      (cond ((eq? m 'acquire) (if (test-and-set!)
                                  (the-mutex 'acquire)
                                  'done))
            ((eq? m 'release) (clear!))
            (else (error "the-mutex -- unrecognized message" m))))
    the-mutex))

; ex 3.47 a
(define (make-semaphore n) ; assumed n >= 1
  (let ((calls 0)
        (mutex (make-mutex)))
    (define (the-semaphore m)
      (cond ((eq? m 'acquire) (begin (mutex 'acquire)
                                     (if (< calls n)
                                         (begin (set! calls (+ calls 1))
                                                (mutex 'release))
                                         (begin (mutex 'release)
                                                (the-semaphore 'acquire)))))

            ((eq? m 'release) (begin (mutex 'acquire)
                                     (if (> calls 0) (set! calls (- calls 1)) 'nothing)
                                     (mutex 'release)))

            (else (error "the-semaphore -- unrecognized message" m))))
    the-semaphore))

; ex 3.47 b
(define (make-semaphore-b n) ; assumed test-and-set! and release! are atomic
  (let ((calls 0))
    (define (test-and-set!) (if (= calls n)
                                #t
                                (begin (set! calls (+ calls 1))
                                       #f)))
    (define (release!) (set! calls (- calls 1)))
    (define (the-semaphore m)
      (cond ((eq? m 'acquire) (if (test-and-set!)
                                  (the-semaphore 'acquire)
                                  'done))
            ((eq? m 'release) (release!))
            (else (error "the-semaphore -- unrecognized message" m))))
    the-semaphore))