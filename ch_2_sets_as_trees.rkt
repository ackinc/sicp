#lang racket
(require (only-in "./ch_2_sets_as_ordered_lists.rkt"
                  [union-set union-set-ordlist]
                  [intersection-set intersection-set-ordlist]))

(define (make-tree entry left-subtree right-subtree) (list entry left-subtree right-subtree))
(define (entry tree) (car tree))
(define (left tree) (cadr tree))
(define (right tree) (caddr tree))

(define (element-of-set? x tree)
  (cond ((null? tree) #f)
        ((= x (entry tree)) #t)
        ((< x (entry tree)) (element-of-set? x (left tree)))
        (else (element-of-set? x (right tree)))))

(define (adjoin-set x tree)
  (cond ((null? tree) (make-tree x null null))
        ((= x (entry tree)) tree)
        ((< x (entry tree)) (make-tree (entry tree) (adjoin-set x (left tree)) (right tree)))
        (else (make-tree (entry tree) (left tree) (adjoin-set x (right tree))))))

(define (append l1 l2) (if (null? l1) l2 (cons (car l1) (append (cdr l1) l2))))
(define (tree->list1 tree)
  (if (null? tree)
      null
      (append (tree->list1 (left tree))
              (cons (entry tree) (tree->list1 (right tree))))))

(define (tree->list2 tree)
  (define (copy-to-list tree result-list)
    (if (null? tree)
        result-list
        (copy-to-list (left tree) (cons (entry tree) (copy-to-list (right tree) result-list)))))
  (copy-to-list tree null))

; convert ordered list to balanced tree
(define (list->tree elts) (car (partial-tree elts (length elts))))

(define (length elts) (if (null? elts) 0 (+ 1 (length (cdr elts)))))
(define (partial-tree elts n)
  (if (= n 0)
      (cons null elts)
      (let ((left-size (quotient (- n 1) 2)))
        (let ((left-result (partial-tree elts left-size)))
          (let ((left-tree (car left-result))
                (non-left-elts (cdr left-result)))
            (let ((this-elem (car non-left-elts))
                  (right-size (- n left-size 1)))
              (let ((right-result (partial-tree (cdr non-left-elts) right-size)))
                (let ((right-tree (car right-result))
                      (remaining-elts (cdr right-result)))
                  (cons (make-tree this-elem left-tree right-tree) remaining-elts)))))))))

; ex 2.65
(define (intersection-set tree1 tree2)
  (let ((set1 (tree->list2 tree1))
        (set2 (tree->list2 tree2)))
    (list->tree (intersection-set-ordlist set1 set2))))

; ex 2.65
(define (union-set tree1 tree2)
  (let ((set1 (tree->list2 tree1))
        (set2 (tree->list2 tree2)))
    (list->tree (union-set-ordlist set1 set2))))

; ex 2.66
(define (lookup key tree)
  (cond ((null? tree) #f)
        ((= key (key (entry tree))) (entry tree))
        ((< key (key (entry tree))) (lookup key (left tree)))
        (else (lookup key (right tree)))))