#lang sicp

(define (attach-tag type-tag contents)
  (cons type-tag contents))

(define (type-tag datum)
  (if (pair? datum)
      (car datum)
      (error "Bad tagged datum: TYPE-TAG" datum)))

(define (contents datum)
  (if (pair? datum)
      (cdr datum)
      (error "Bad tagged datum: CONTENTS" datum)))

;; Dummy procedures
(define (put op type item)
  (error "Not implemented: PUT" op type item))

(define (get op type)
  (error "Not implemented: GET" op type))

(define (get-record name file)
  ((get 'get-record (type-tag file)) name file))

(define (get-salary record)
  ((get 'get-salary (type-tag record)) record))

(define (find-employee-record name . files)
  (if (null? files)
      false
      (let ((record
             (get-record name (car files))))
        (if record
            record
            (apply find-employee-record
                   name
                   (cdr files))))))
