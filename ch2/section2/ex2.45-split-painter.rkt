#lang sicp

(define (split first second)
  (define (split-impl painter n)
    (if (= n 0)
        painter
        (let ((smaller (split-impl painter
                                   (- n 1))))
          (first painter
                 (second smaller smaller)))))
  (lambda (painter n)
    (split-impl painter n)))

(define right-split (split beside below))
(define up-split (split below beside))
