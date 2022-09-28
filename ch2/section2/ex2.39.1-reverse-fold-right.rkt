#lang sicp

(define (fold-right op initial sequence)
  (if (null? sequence)
      initial
      (op (car sequence)
          (fold-right op
                      initial
                      (cdr sequence)))))

; Push elem to the end of sequence. Example:
; > (push 4 (list 1 2 3))
; (1 2 3 4)
(define (push elem sequence)
  (append sequence (list elem)))

; For a sequence (x1 x2 ... xn) fold-right 'accumulates' like this:
; (op x1 (op x2 (op ... (op xn initial) ... )))
; Reverse: (x1 x2 ... xn) -> (xn ... x2 x1)
; To reverse a sequence using fold-right:
; Fold-right will combine the first element of the sequence with the
; 'accumulation' of the rest. Here the accumulation will be an already
; reversed list (not yet until we decide the op), we want to 'push' the
; first element to the end of the already reversed list.
; Thus, op is push and initial is nil.
; (push x1 (push x2 (push ... (push xn nil) ... )))
; We can see how the fold right with these arguments will produce the
; reverse.
(define (reverse sequence)
  (fold-right push nil sequence))

(reverse nil)
(reverse (list 1))
(reverse (list 1 2))
(reverse (list 1 2 3))
