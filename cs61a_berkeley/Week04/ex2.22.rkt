#lang racket

(require berkeley)

(define (square-list items)
  (define (iter things answer)
    (if (null? things)
        answer
        (iter (cdr things)
              (cons (square (car things))
                    answer))))
  (iter items nil))

;; Square-list produces list in reverse because it starts with the first
;; element, square it and cons it to the answer. When doing the same thing to
;; the second element the call to cons adds the square to the front rather
;; than at the back.

(define (square-list2 items)
  (define (iter things answer)
    (if (null? things)
        answer
        (iter (cdr things)
              (cons answer
                    (square
                     (car things))))))
  (iter items nil))

;; Square-list2 doesn't work either as the call to cons creates a pair with
;; the car being answer and the cdr being the square of the element. This does
;; not create a list, but a deep pair like thing.
