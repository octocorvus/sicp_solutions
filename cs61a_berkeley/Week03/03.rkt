#lang racket

(require berkeley)

(define (cc amount coins)
  (cond ((zero? amount) 1)
        ((or (negative? amount)
             (empty? coins))
         0)
        (else (+ (cc (- amount (first coins)) coins)
                 (cc amount (butfirst coins))))))

(define (cc-flip amount coins)
  (cond ((or (negative? amount)
             (empty? coins))
         0)
        ((zero? amount) 1)
        (else (+ (cc (- amount (first coins)) coins)
                 (cc amount (butfirst coins))))))

;; cc and cc-flip will behave differently only when
;; amount is 0 and coins is an empty sentence.
(cc 0 '())
(cc-flip 0 '())
