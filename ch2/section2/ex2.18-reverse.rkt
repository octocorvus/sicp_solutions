#lang sicp

(define (reverse list-instance)
  (define (iter result list-instance)
    (if (null? list-instance)
        result
        (iter (cons (car list-instance)
                    result)
              (cdr list-instance))))
  (iter (list) list-instance))

(define (reverse-rec list-instance)
  (if (null? list-instance)
      list-instance
      (append (reverse-rec (cdr list-instance))
              (list (car list-instance)))))

(reverse (list 1 4 9 16 25))
(reverse-rec (list 1 4 9 16 25))
