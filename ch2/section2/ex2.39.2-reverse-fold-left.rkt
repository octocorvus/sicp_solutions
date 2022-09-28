#lang sicp

(define (fold-left op initial sequence)
  (define (iter result rest)
    (if (null? rest)
        result
        (iter (op result (car rest))
              (cdr rest))))
  (iter initial sequence))

; For a sequence (x1 x2 ... xn), fold-left 'accumulates' elements in the
; reverse order as that of fold-right:
; (op ... (op (op initial x1) x2) ... xn)
; Reverse: (x1 x2 ... xn) -> (xn ... x2 x1)
; We can reverse a list like:
; (cons xn ... (cons x2 (cons x1 initial)) ... )
; To do that with fold-left, we pass it a procedure that makes a pair, like
; cons but in reverse order: (lambda (x y) (cons y x))
(define (reverse sequence)
  (fold-left
   (lambda (x y) (cons y x))
   nil sequence))

(reverse nil)
(reverse (list 1))
(reverse (list 1 2))
(reverse (list 1 2 3))
