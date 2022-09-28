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

(define (variable? x)
  ; The variables are symbols.
  (symbol? x))

(define (same-variable? v1 v2)
  ; Two variables are the same if the symbols representing them
  ; are 'eq?.
  (and (variable? v1)
       (variable? v2)
       (eq? v1 v2)))

;; Dummy procedures
(define (put op type item)
  (error "Not implemented: PUT" op type item))

(define (get op type)
  (error "Not implemented: GET" op type))

(define (deriv exp var)
  (cond ((number? exp) 0)
        ((variable? exp)
         (if (same-variable? exp var)
             1
             0))
        (else ((get 'deriv (operator exp))
               (operands exp)
               var))))

(define (operator exp) (car exp))
(define (operands exp) (cdr exp))

(define (install-sum-package)
  ;; internal procedures
  (define (make-sum a1 a2) (list a1 a2))
  (define (addend e) (car e))
  (define (augend e) (cadr e))
  ;; interface to the rest of the system
  (define (tag x) (attach-tag '+ x))
  (put 'make-sum '+
       (lambda (a1 a2)
         (tag (make-sum a1 a2))))
  (put 'deriv '+
       (let ((make-sum (get 'make-sum '+)))
         (lambda (exp var)
           (make-sum
            (deriv (addend exp) var)
            (deriv (augend exp) var)))))
  'done)

(define (install-product-package)
  ;; internal procedures
  (define (make-product m1 m2) (list m1 m2))
  (define (multiplier p) (car p))
  (define (multiplicand p) (cadr p))
  ;; interface to the rest of the system
  (define (tag x) (attach-tag '* x))
  (if (not (get 'make-sum '+))
      (install-sum-package)
      '())
  (put 'make-product '*
       (lambda (m1 m2)
         (tag (make-product m1 m2))))
  (put 'deriv '*
       (let ((make-sum (get 'make-sum '+))
             (make-product
              (get 'make-product '*)))
         (lambda (exp var)
           (make-sum
            (make-product
             (multiplier exp)
             (deriv (multiplicand exp) var))
            (make-product
             (deriv (multiplier exp) var)
             (multiplicand exp))))))
  'done)

(define (install-exponentiation-package)
  ;; internal procedures
  (define (make-exponentiation b n) (list b n))
  (define (base e) (car e))
  (define (exponent e) (cadr e))
  ;; interface to the rest of the system
  (define (tag x) (attach-tag '** x))
  (if (not (get 'make-product '*))
      (install-product-package)
      '())
  (put 'make-exponentiation '**
       (lambda (b n)
         (tag (make-exponentiation b n))))
  (put 'deriv '**
       (let ((make-product
              (get 'make-product '*))
             (make-exponentiation
              (get 'make-exponentiation '**)))
         (lambda (exp var)
           (let ((b (base exp))
                 (n (exponent exp)))
             (make-product
              (make-product
               n
               (make-exponentiation b (- n 1)))
              (deriv b var))))))
  'done)
