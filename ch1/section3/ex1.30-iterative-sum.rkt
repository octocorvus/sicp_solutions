#lang sicp

; Iterative sum

(define (sum term a next b)
  (define (iter a result)
    (if (> a b)
        result
        (iter (next a) (+ result (term a)))))
  (iter a 0))

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
