#lang sicp

; Applies proc to each element of seq and append the
; results.
(define (flatmap proc sequence)
  (accumulate append nil (map proc sequence)))

; Combines elements of the sequence using 'op' given
; a null value 'initial'.
(define (accumulate op initial sequence)
  (if (null? sequence)
      initial
      (op (car sequence)
          (accumulate op
                      initial
                      (cdr sequence)))))

; Return list of unique pairs of positive integers
; less than or equal to n.
(define (unique-pairs n)
  (flatmap (lambda (i)
             (map (lambda (j)
                    (list i j))
                  (enumerate-interval 1 (- i 1))))
           (enumerate-interval 1 n)))

; Return list of unique triples of positive integers
; less than or equal to n.
(define (unique-triples n)
  (flatmap (lambda (i)
             (map (lambda (pair)
                    (cons i pair))
                  (unique-pairs (- i 1))))
           (enumerate-interval 1 n)))

; Return list of all numbers between low and high,
; both inclusive.
(define (enumerate-interval low high)
  (if (> low high)
      nil
      (cons low
            (enumerate-interval
             (+ low 1)
             high))))

; Return the sum of all elements of a sequence.
(define (sum sequence)
  (accumulate + 0 sequence))

; Check if sum of all elements of a sequence is equal
; to a given number s.
(define (equal-sum? s sequence)
  (= s (sum sequence)))

; Filter elements of a sequence using a given predicate.
(define (filter predicate sequence)
  (cond ((null? sequence) nil)
        ((predicate (car sequence))
         (cons (car sequence)
               (filter predicate
                       (cdr sequence))))
        (else (filter predicate
                      (cdr sequence)))))

; Return all unique triples of positive integers less
; than or equal to n, whose sum is equal to a given
; integer s.
(define (sum-triples n s)
  (filter (lambda (sequence)
            (equal-sum? s sequence))
          (unique-triples n)))

(sum-triples 6 10)
(sum-triples 42 69)
