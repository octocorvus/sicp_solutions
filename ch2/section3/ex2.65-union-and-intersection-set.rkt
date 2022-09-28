#lang sicp

(define (entry tree) (car tree))

(define (left-branch tree) (cadr tree))

(define (right-branch tree) (caddr tree))

(define (make-tree entry left right)
  (list entry left right))

(define (list->tree elements)
  (car (partial-tree
        elements (length elements))))

(define (partial-tree elts n)
  (if (= n 0)
      (cons '() elts)
      (let ((left-size
             (quotient (- n 1) 2)))
        (let ((left-result
               (partial-tree
                elts left-size)))
          (let ((left-tree
                 (car left-result))
                (non-left-elts
                 (cdr left-result))
                (right-size
                 (- n (+ left-size 1))))
            (let ((this-entry
                   (car non-left-elts))
                  (right-result
                   (partial-tree
                    (cdr non-left-elts)
                    right-size)))
              (let ((right-tree
                     (car right-result))
                    (remaining-elts
                     (cdr right-result)))
                (cons (make-tree this-entry
                                 left-tree
                                 right-tree)
                      remaining-elts))))))))

(define (tree->list tree)
  (define (copy-to-list tree result-list)
    (if (null? tree)
        result-list
        (copy-to-list
         (left-branch tree)
         (cons (entry tree)
               (copy-to-list
                (right-branch tree)
                result-list)))))
  (copy-to-list tree '()))

(define (union-set set1 set2)
  (define (union-set-list set1 set2)
    (cond ((null? set1) set2)
          ((null? set2) set1)
          ((= (car set1) (car set2))
           (cons (car set1)
                 (union-set-list
                  (cdr set1)
                  (cdr set2))))
          ((< (car set1) (car set2))
           (cons (car set1)
                 (union-set-list
                  (cdr set1)
                  set2)))
          ((> (car set1) (car set2))
           (cons (car set2)
                 (union-set-list
                  set1
                  (cdr set2))))))
  (list->tree
   (union-set-list
    (tree->list set1)
    (tree->list set2))))

(define (intersection-set set1 set2)
  (define (intersection-set-list set1 set2)
    (cond ((or (null? set1) (null? set2))
           '())
          ((= (car set1) (car set2))
           (cons (car set1)
                 (intersection-set-list
                  (cdr set1)
                  (cdr set2))))
          ((< (car set1) (car set2))
           (intersection-set-list
            (cdr set1)
            set2))
          ((> (car set1) (car set2))
           (intersection-set-list
            set1
            (cdr set2)))))
  (list->tree
   (intersection-set-list
    (tree->list set1)
    (tree->list set2))))
