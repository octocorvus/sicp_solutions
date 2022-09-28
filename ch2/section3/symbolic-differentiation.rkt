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
        ((difference? exp)
         (make-difference (deriv (addend exp) var)
                          (deriv (augend exp) var)))
        ((product? exp)
         (make-sum
          (make-product
           (multiplier exp)
           (deriv (multiplicand exp) var))
          (make-product
           (deriv (multiplier exp) var)
           (multiplicand exp))))
        ((division? exp)
         (let ((u (dividend exp))
               (v (divisor exp)))
           (make-division
            (make-difference (make-product v (deriv u var))
                             (make-product u (deriv v var)))
            (make-exponentiation v 2))))
        ((exponentiation? exp)
         (let ((b (base exp))
               (n (exponent exp)))
           (if (>=number? n 0)
               (make-product
                n
                (make-exponentiation b (make-difference n 1))
                (deriv b var))
               (error
                "I don't know how to differentiate that yet: DERIV"
                exp))))
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

(define (make-sum a1 a2 . terms)
  (define (make-sum-impl sum-numbers terms)
    (cond ((null? terms)
           (if (> sum-numbers 0)
               (list sum-numbers)
               '()))
          ((number? (car terms))
           (make-sum-impl (+ sum-numbers
                             (car terms))
                          (cdr terms)))
          (else
           (cons (car terms)
                 (make-sum-impl sum-numbers
                                (cdr terms))))))
  (let ((sum (make-sum-impl
              0
              (append (list a1 a2) terms))))
    (if (null? (cdr sum))
        (car sum)
        (cons '+ sum))))

(define (make-difference a1 a2)
  (cond ((=number? a1 0) a2)
        ((=number? a2 0) a1)
        ((and (number? a1) (number? a2))
         (- a1 a2))
        ((eq? a1 a2) 0)
        (else (list '- a1 a2))))

(define (make-product m1 m2 . terms)
  (define (make-product-impl product-numbers terms)
    (cond ((null? terms)
           (if (= product-numbers 1)
               '()
               (list product-numbers)))
          ((number? (car terms))
           (make-product-impl (* product-numbers
                                 (car terms))
                              (cdr terms)))
          (else
           (cons (car terms)
                 (make-product-impl product-numbers
                                    (cdr terms))))))
  (let ((product (make-product-impl
                  1
                  (append (list m1 m2) terms))))
    (cond ((contains? 0 product)
           0)
          ((null? (cdr product))
           (car product))
          (else (cons '* product)))))

(define (make-division m1 m2)
  (cond ((=number? m2 0)
         (error "Division by zero: MAKE-DIVISION" m1 m2))
        ((=number? m1 0) 0)
        ((=number? m2 1) m1)
        ((and (number? m1) (number? m2))
         (/ m1 m2))
        (else (list '/ m1 m2))))

(define (make-exponentiation b n)
  (cond ((=number? n 0) 1)
        ((=number? n 1) b)
        ((or (=number? b 0) (=number? b 1))
         b)
        ((and (number? b) (number? n))
         (expt b n))
        (else (list '** b n))))

; Check whether an expression is equal to a given number.
(define (=number? exp num)
  (and (number? exp) (= exp num)))

; Check whether an expression is greater than or equal to a given number.
(define (>=number? exp num)
  (and (number? exp) (not (< exp num))))

(define (sum? x)
  ; A sum is a list whose first element is the symbol '+.
  (and (pair? x) (eq? (car x) '+)))

(define (addend s) (cadr s))

(define (augend s)
  (let ((rest (cddr s)))
    (if (null? (cdr rest))
        (car rest)
        (cons '+ rest))))

(define (difference? x)
  ; A difference is a list whose first element is the symbol '-.
  (and (pair? x) (eq? (car x) '-)))

(define (minuend s) (cadr s))

(define (subtrahend s) (caddr s))

(define (product? x)
  ; A product is a list whose first element is the symbol '*.
  (and (pair? x) (eq? (car x) '*)))

(define (multiplier p) (cadr p))

(define (multiplicand p)
  (let ((rest (cddr p)))
    (if (null? (cdr rest))
        (car rest)
        (cons '* rest))))

(define (division? x)
  ; A division is a list whose first element is the symbol '/.
  (and (pair? x) (eq? (car x) '/)))

(define (dividend x) (cadr x))

(define (divisor x) (caddr x))

(define (exponentiation? x)
  ; An exponentiation is a list whose first element is the symbol '**.
  (and (pair? x) (eq? (car x) '**)))

(define (base e) (cadr e))

(define (exponent e) (caddr e))

; Check if an element is present in a list.
(define (contains? elem a-list)
  (if (memq elem a-list) #t #f))

; Examples.
(deriv '(+ x 3) 'x)

(deriv '(* x y) 'x)

(deriv '(* (* x y) (+ x 3)) 'x)

(deriv '(** x 3) 'x)

(deriv '(+ (* y (** x 3)) (* (** z 2) (** x 2))) 'x)

(deriv '(/ (** x 3) (+ (* y x) (** x 2))) 'x)
