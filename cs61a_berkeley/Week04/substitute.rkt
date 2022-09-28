#lang racket

(require berkeley)

(define (substitute list-or-word old-word new-word)
  (cond ((list? list-or-word)
         (map (lambda (list-or-word)
                (substitute list-or-word old-word new-word))
              list-or-word))
        ((equal? list-or-word old-word) new-word)
        (else list-or-word)))

(define (substitute2 list-or-word old-words new-words)
  (cond ((list? list-or-word)
         (map (lambda (list-or-word)
                (substitute2 list-or-word old-words new-words))
              list-or-word))
        ((member? list-or-word old-words)
         (list-ref new-words
                   (index-of old-words list-or-word)))
        (else list-or-word)))
