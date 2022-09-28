#lang sicp

(define (deep-reverse x)
  (define (iter result x)
    (cond ((null? x)
           result)
          ((not (pair? x))
           x)
          (else
           (iter (cons (deep-reverse (car x))
                       result)
                 (cdr x)))))
  (iter nil x))

(define (reverse list-instance)
  (define (iter result list-instance)
    (if (null? list-instance)
        result
        (iter (cons (car list-instance)
                    result)
              (cdr list-instance))))
  (iter (list) list-instance))

(define (deep-reverse-rec x)
  (if (or (null? x)
          (not (pair? x)))
      x
      (append (deep-reverse (cdr x))
              (list (deep-reverse (car x))))))

(define (reverse-rec list-instance)
  (if (null? list-instance)
      list-instance
      (append (reverse-rec (cdr list-instance))
              (list (car list-instance)))))

(define x
  (list (list 1 2) (list 3 4)))

x

(reverse-rec x)

(deep-reverse x)
