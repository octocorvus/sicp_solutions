;; Scheme calculator -- evaluate simple expressions

#lang racket

; The read-eval-print loop:

(define (calc)
  (display "calc: ")
  (flush-output)
  (let ((val (calc-eval (read))))
    (cond ((eq? val 'exit) (newline))
          (else (print val)
                (newline)
                (calc)))))

; Evaluate an expression:

(define (calc-eval exp)
  (cond ((eof-object? exp) 'exit)
        ((number? exp) exp)
        ((or (eq? exp 'pi)
             (eq? exp 'π))
         3.14159265359)
        ((list? exp) (calc-apply (car exp) (map calc-eval (cdr exp))))
        (else (error "CALC-EVAL: bad expression:" exp))))

; Apply a function to arguments:

(define (calc-apply fn args)
  (cond ((eq? fn '+) (accumulate + 0 args))
        ((eq? fn '-) (cond ((null? args) (error "CALC-APPLY: no args to -"))
                           ((= (length args) 1) (- (car args)))
                           (else (- (car args) (accumulate + 0 (cdr args))))))
        ((eq? fn '*) (accumulate * 1 args))
        ((eq? fn '/) (cond ((null? args) (error "CALC-APPLY: no args to /"))
                           ((= (length args) 1) (/ (car args)))
                           (else (/ (car args) (accumulate * 1 (cdr args))))))
        ((eq? fn '**) (if (= (length args) 2)
                          (expt (car args) (cadr args))
                          (error "CALC-APPLY: exponent requires 2 arguments")))
        (else (error "CALC-APPLY: bad operator:" fn))))

(define (accumulate proc null-value sequence)
  (define (iter result sequence)
    (if (null? sequence)
        result
        (iter (proc result (car sequence))
              (cdr sequence))))
  (iter null-value sequence))

(calc)
