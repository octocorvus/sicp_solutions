#lang racket

(require berkeley)

(define (switch-rec sent is-first)
  (cond ((empty? sent) '())
        ((member? (first sent) '(I Me me))
         (se 'you (switch-rec (bf sent) false)))
        ((member? (first sent) '(You you))
         (if is-first
             (se 'I (switch-rec (bf sent) false))
             (se 'me (switch-rec (bf sent) false))))
        (else (se (first sent)
                  (switch-rec (bf sent) false)))))

(define (switch sent)
  (switch-rec sent true))

(switch '(You told me that I should wake you up))
