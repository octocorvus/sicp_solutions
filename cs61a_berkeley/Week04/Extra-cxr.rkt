#lang racket

(require berkeley)

(define (cxr-function cxr)
  (define (xr-function xr)
    (let ((op (first xr))
          (rest-of-ops (butfirst xr)))
      (cond ((equal? op 'r)
             identity)
            ((equal? op 'a)
             (lambda (pair)
               (car ((xr-function rest-of-ops) pair))))
            ((equal? op 'd)
             (lambda (pair)
               (cdr ((xr-function rest-of-ops) pair))))
            (else
             (error "XR-FUNCTION: Invalid cxr operation:" op cxr)))))
  (if (equal? (first cxr) 'c)
      (xr-function (butfirst cxr))
      (error "CXR-FUNCTION: cxr string doesn't start with c:" cxr)))
