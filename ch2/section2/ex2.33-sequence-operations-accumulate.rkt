#lang sicp

(define (accumulate op initial sequence)
  (if (null? sequence)
      initial
      (op (car sequence)
          (accumulate op
                      initial
                      (cdr sequence)))))

(define (map p sequence)
  (accumulate (lambda (x y) (cons (p x) y))
              nil sequence))

(map inc nil)
(map inc (list 1))
(map inc (list 1 2 3 4 5))
(map (lambda (x) (* x x x)) (list 1 2 3 4 5))
(map (lambda (x)
       (/ (log x) (log 10)))
     (list 10 100 1000 10000 100000))

(define (append seq1 seq2)
  (accumulate cons seq2 seq1))

(append (map inc (list 1 2 3 4 5))
        (map (lambda (x) (* x x x)) (list 1 2 3 4 5)))
(append (map (lambda (x) (* x x x)) (list 1 2 3 4 5))
        (map (lambda (x)
               (/ (log x) (log 10)))
             (list 10 100 1000 10000 100000)))
(append (list 1) nil)
(append nil (list 1 2))
(append nil nil)

(define (length sequence)
  (accumulate (lambda (x y) (inc y))
              0 sequence))

(length (append (map inc (list 1 2 3 4 5))
        (map (lambda (x) (* x x x)) (list 1 2 3 4 5))))
(length (append (map (lambda (x) (* x x x)) (list 1 2 3 4 5))
        (map (lambda (x)
               (/ (log x) (log 10)))
             (list 10 100 1000 10000 100000))))
(length nil)
(length (list 1))
