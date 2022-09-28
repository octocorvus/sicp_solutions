#lang sicp

; Does what you think.
(define (square x)
  (* x x))

; Return the smallest divisor of n.
(define (smallest-divisor n)
  (find-divisor n 2))

(define (find-divisor n test-divisor)
  (cond ((> (square test-divisor) n)
         n)
        ((divides? test-divisor n)
         test-divisor)
        (else (find-divisor
               n
               (+ test-divisor 1)))))

(define (divides? a b)
  (= (remainder b a) 0))

(define (prime? n)
  (= n (smallest-divisor n)))

; Return list of all integers between low and high, both inclusive.
(define (enumerate-interval low high)
  (if (> low high)
      nil
      (cons low
            (enumerate-interval
             (+ low 1)
             high))))

; Return list of elements that passes the given predicate from given
; sequence.
(define (filter predicate sequence)
  (cond ((null? sequence) nil)
        ((predicate (car sequence))
         (cons (car sequence)
               (filter predicate
                       (cdr sequence))))
        (else (filter predicate
                      (cdr sequence)))))

; Combines elements of the sequence using 'op' given a null value 'initial'.
(define (accumulate op initial sequence)
  (if (null? sequence)
      initial
      (op (car sequence)
          (accumulate op
                      initial
                      (cdr sequence)))))

; Applies proc to each element of seq and append the results.
(define (flatmap proc seq)
  (accumulate append nil (map proc seq)))

; Check if the sum of the elements of a pair is prime.
(define (prime-sum? pairs)
  (prime? (+ (car pairs) (cadr pairs))))

; Add sum of the elements of pair to the end of the list.
(define (make-pair-sum pairs)
  (list (car pairs)
        (cadr pairs)
        (+ (car pairs) (cadr pairs))))

; Return the list (i, j, i+j) such that 1 <= j < i <= n and i+j is prime.
(define (prime-sum-pairs n)
  (map make-pair-sum
       (filter
        prime-sum?
        (flatmap
         (lambda (i) ; for each i in (1 ... n), we enumerate intergers j<i
           (map (lambda (j) ; for each j in (1 ... i-1), we make list (i, j)
                  (list i j))
                (enumerate-interval
                 1
                 (- i 1))))
         (enumerate-interval 1 n)))))

(prime-sum-pairs 6)

; Return list of permutations of a set S.
(define (permutations s)
  (if (null? s)  ; empty set?
      (list nil) ; sequence containing empty set
      (flatmap (lambda (x) ; for each x in s, we find permutations of s\x
                           ; that begins with x
                 (map (lambda (p)   ; for each permutation of s\x
                        (cons x p)) ; we adjoin x to the front
                      (permutations
                       (remove x s))))
               s)))

; Remove all occurences of an item from a sequence.
(define (remove item sequence)
  (filter (lambda (x) (not (= x item)))
          sequence))

(permutations (list 1 2 3))
