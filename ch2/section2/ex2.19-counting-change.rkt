#lang sicp

(define (count-change amount coin-values)
  (define (cc amount coin-values)
    (cond ((= amount 0)
           1)
          ((or (< amount 0)
               (no-more? coin-values))
           0)
          (else
           (+ (cc
               amount
               (except-first-denomination
                coin-values))
              (cc
               (- amount
                  (first-denomination
                   coin-values))
               coin-values)))))
  (define (first-denomination coin-values)
    (car coin-values))
  (define (no-more? coin-values)
    (null? coin-values))
  (define (except-first-denomination
           coin-values)
    (cdr coin-values))
  (cc amount coin-values))

(define us-coins
  (list 50 25 10 5 1))

(define uk-coins
  (list 50 20 10 100 5 2 1 0.5))

(count-change 10 us-coins)
(count-change 100 us-coins)
(count-change 10 uk-coins)
(count-change 100 uk-coins)
