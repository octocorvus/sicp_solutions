#lang sicp

; Calculate derivate of a given algebraic expression with respect
; to a given variable.
(define (deriv exp var)
  (cond ((number? exp) 0)
        ((variable? exp)
         (if (same-variable? exp var) 1 0))
        ((sum? exp)
         (make-sum (deriv (addend exp) var)
                   (deriv (augend exp) var)))
        ((product? exp)
         (make-sum
          (make-product
           (multiplier exp)
           (deriv (multiplicand exp) var))
          (make-product
           (deriv (multiplier exp) var)
           (multiplicand exp))))
        (else (error "unknown expression type: DERIV" exp))))

(define (variable? x)
  ; The variables are symbols.
  (symbol? x))

(define (same-variable? v1 v2)
  ; Two variables are the same if the symbols representing them
  ; are 'eq?.
  (and (variable? v1)
       (variable? v2)
       (eq? v1 v2)))

(define (make-sum a1 a2)
  (cond ((=number? a1 0) a2)
        ((=number? a2 0) a1)
        ((and (number? a1) (number? a2))
         (+ a1 a2))
        (else (list a1 '+ a2))))

(define (make-product m1 m2)
  (cond ((or (=number? m1 0)
             (=number? m2 0))
         0)
        ((=number? m1 1) m2)
        ((=number? m2 1) m1)
        ((and (number? m1) (number? m2))
         (* m1 m2))
        (else (list m1 '* m2))))

; Check whether an expression is equal to a given number.
(define (=number? exp num)
  (and (number? exp) (= exp num)))

(define (sum? x)
  ; A sum is a list which contains the symbol '+.
  (if (break x '+) #t #f))

(define (addend s)
  (let ((exp (car (break s '+))))
    (if (null? (cdr exp))
        (car exp)
        exp)))

(define (augend s)
  (let ((exp (cadr (break s '+))))
    (if (null? (cdr exp))
        (car exp)
        exp)))

(define (product? x)
  ; A product is a list which is not a sum and contains the symbol '*.
  (if (and (not (sum? x))
           (break x '*))
      #t
      #f))

(define (multiplier p)
  (let ((exp (car (break p '*))))
    (if (null? (cdr exp))
        (car exp)
        exp)))

(define (multiplicand p)
  (let ((exp (cadr (break p '*))))
    (if (null? (cdr exp))
        (car exp)
        exp)))

; Break the list s into two halves at a given element x. Return #f if
; element doesn't exist.
(define (break s x)
  (define (iter first-half second-half)
    (cond ((null? second-half)
           #f)
          ((equal? (car second-half) x)
           (list first-half
                 (cdr second-half)))
          (else
           (iter (append first-half
                         (list (car second-half)))
                 (cdr second-half)))))
  (iter '() s))

; Examples.
(deriv '(x + 3) 'x)

(deriv '(x * y) 'x)

(deriv '((x * y) * (x + 3)) 'x)

(deriv '(x + (3 * (x + (y + 2)))) 'x)

(deriv '(x + 3 * (x + y + 2)) 'x)
