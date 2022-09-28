#lang racket

(require berkeley)

(define (for-each fn a-list)
  (cond ((not (null? a-list))
         (fn (car a-list))
         (for-each fn (cdr a-list)))))

(for-each
 (lambda (x) (newline) (display x))
 (list 57 321 88))
