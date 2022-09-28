#lang sicp

(define (sum term a next b)
  (if (> a b)
      0
      (+ (term a)
         (sum term (next a) next b))))

(define (integral f a b n)
  (let ((h (/ (- b a) n)))
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
          (f b)))))

(define (cube x) (* x x x))

(integral cube 0 1 100)
(integral cube 0 1 1000)
(integral cube 4 5 1000)
