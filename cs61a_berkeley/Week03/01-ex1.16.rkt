#lang racket

(define (square x) (* x x))

(define (fast-expt b n)
  (define (iter result base expnt)
    (cond ((zero? expnt) result)
          ((even? expnt)
           (iter result (square base) (/ expnt 2)))
          (else
           (iter (* base result) base (sub1 expnt)))))
  (iter 1 b n))

;; Test fast-expt against results of expt.

(define (test-fast-expt)
  (define (make-status success msg)
    (cons success msg))
  (define (success status)
    (car status))
  (define (msg status)
    (cdr status))
  (define (test fn args result)
    (let ((output (apply fn args)))
      (make-status (equal? output result)
                   (list 'output: output 'result: result))))
  (define (iter n)
    (let* ((min_int -1000)
           (max_int 1000)
           (max_uint 2000)
           (base (random min_int max_int))
           (expnt (random max_uint))
           (result (expt base expnt))
           (status (test fast-expt
                         (list base expnt)
                         result)))
      (cond ((zero? n) (make-status true "All tests passed"))
            ((success status)
             (iter (sub1 n)))
            (else
             (make-status
              false (cons "Some tests failed" (msg status)))))))
  (display (msg (iter 10000))))

(test-fast-expt)
