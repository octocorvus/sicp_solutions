#lang sicp

(define (timed-prime-test n)
  (newline)
  (display n)
  (start-prime-test n (runtime)))

(define (start-prime-test n start-time)
  (if (prime? n)
      (report-prime (- (runtime)
                       start-time))))

(define (report-prime elapsed-time)
  (display " *** ")
  (display elapsed-time))

(define (search-for-primes start end)
  (define (iter counter step)
    (cond ((< counter end)
           (timed-prime-test counter)
           (iter (+ counter step) step))))
  (if (even? start)
      (iter (+ start 1) 2)
      (iter start 2)))

(define (square x)
  (* x x))

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

(display "Finding three smallest primes larger than 1000")
(search-for-primes 1000 1050)
(display "\nFinding three smallest primes larger than 10000")
(search-for-primes 10000 10050)
(display "\nFinding three smallest primes larger than 100000")
(search-for-primes 100000 100050)
(display "\nFinding three smallest primes larger than 1000000")
(search-for-primes 1000000 1000050)
