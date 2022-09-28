#lang sicp

(define (accumulate
         combiner null-value term a next b)
  (define (iter a result)
    (if (> a b)
        result
        (iter (next a)
              (combiner result
                        (term a)))))
  (iter a null-value))

; Sum and product in terms of accumulate
(define (sum term a next b)
  (accumulate + 0 term a next b))

(define (product term a next b)
  (accumulate * 1 term a next b))

;; Testing sum
; To define sum-cubes

(define (cube x) (* x x x))

(define (inc n) (+ n 1))

(define (sum-cubes a b)
  (sum cube a inc b))

(sum-cubes 1 10)

; To define sum-integers

(define (identity x) x)

(define (sum-integers a b)
  (sum identity a inc b))

(sum-integers 1 10)

; To define pi-sum

(define (pi-sum a b)
  (define (pi-term x)
    (/ 1.0 (* x (+ x 2))))
  (define (pi-next x)
    (+ x 4))
  (sum pi-term a pi-next b))

(* 8 (pi-sum 1 1000))

(define (integral f a b n)
  (define h (/ (- b a) n))
  (define (add-2h x) (+ x (* 2 h)))
  (* (/ h 3.0)
     (+ (* 4 (sum f
                  (+ a h)
                  add-2h
                  (+ a (* (- n 1) h))))
        (* 2 (sum f
                  (add-2h a)
                  add-2h
                  (+ a (* (- n 2) h))))
        (f a)
        (f b))))

(integral cube 0 1 100)
(integral cube 0 1 1000)
(integral cube 4 5 1000)

;; Testing product
; Product of integers a through b

(define (product-integers a b)
  (product identity a inc b))

(product-integers 1 4)

; Factorial
(define (factorial n)
  (product identity 1 inc n))

(factorial 5)
(factorial 30)

; Approximation of pi
(define (square x) (* x x))

(define (reciprocal x) (/ 1 x))

(define (pi-product a b)
  (define (reciprocal-inc x)
    (if (< x 1)
        (inc (reciprocal x))
        (reciprocal (inc x))))
  (define (calc-product a b)
    (cond ((even? a)
           (calc-product (+ a 1) b))
          ((even? b)
           (calc-product a (- b 1)))
          (else
           (product square (reciprocal a) reciprocal-inc b))))
  (* 4.0
     a
     b
     (calc-product a b)))

(pi-product 2 1000)

; Better and correct version
; Kept the old version for laughs and shits
(define (wallis-product n)
  (define (term x)
    (/ 1 (- 1 (/ 1 (* 4 (square x))))))
  (* 2.0 (product term 1 inc n)))

(wallis-product 10000)
