#lang sicp

(define (lookup given-key set-of-records)
  (cond ((null? set-of-records) false)
        ((equal? given-key
                 (key (entry set-of-records)))
         (entry set-of-records))
        ((< given-key
            (key (entry set-of-records)))
         (lookup given-key
                 (left-branch set-of-records)))
        ((> given-key
            (key (entry set-of-records)))
         (lookup given-key
                 (right-branch set-of-records)))))

(define (make-record key value)
  (list key value))

(define (key record)
  (car record))

(define (value record)
  (cadr record))

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

(define test-records
  (list->tree
   (list
    (make-record 1 "apple")
    (make-record 2 "banana")
    (make-record 3 "orange")
    (make-record 4 "pineapple")
    (make-record 5 "avocado")
    (make-record 6 "grapes")
    (make-record 7 "pomengrate")
    (make-record 8 "papaya")
    (make-record 9 "coconut")
    (make-record 10 "mulberry")
    (make-record 11 "mango")
    (make-record 12 "dragon fruit")
    (make-record 13 "guava"))))

(lookup 1 test-records)
(lookup 2 test-records)
(lookup 3 test-records)
(lookup 4 test-records)
(lookup 5 test-records)
(lookup 6 test-records)
(lookup 7 test-records)
(lookup 8 test-records)
(lookup 9 test-records)
(lookup 10 test-records)
(lookup 11 test-records)
(lookup 12 test-records)
(lookup 13 test-records)
(lookup 14 test-records)
(lookup 99 test-records)
