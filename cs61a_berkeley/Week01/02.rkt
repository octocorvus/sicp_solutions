#lang racket

(require berkeley)

(define (squares numbers)
  (if (empty? numbers)
      '()
      (sentence (square (first numbers))
                (squares (butfirst numbers)))))

(define (square number)
  (* number number))

(squares '(1 2 3 4 5 6 7 8 9 10))
