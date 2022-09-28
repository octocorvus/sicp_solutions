#lang racket

(require berkeley)

;; Recursion without define.

((lambda (n)
   ((lambda (fact)
      (fact fact n))
    (lambda (fact n)
      (if (= n 0)
          1
          (* n (fact fact (- n 1)))))))
 5)

;; I fucking did it!
