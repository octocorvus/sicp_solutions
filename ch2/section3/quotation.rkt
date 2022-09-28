#lang sicp

(define a 1)
(define b 2)

(list a b)   ; (1 2)
(list 'a 'b) ; (a b)
(list 'a b)  ; (a 2)

; Quotation also allows us to type in compound objects,
; using the conventional printed representation for lists:
(car '(a b c)) ; a
(cdr '(a b c)) ; (b c)

; We can obtain empty lists by evaluating '(), and thus
; dispense with the variable nil.
'()

; To maintain the consistency that all compound expressions
; in our language should be delimited by parentheses and
; look like lists -- we introduce a special form "quote,"
; which serves as the same purpose as the quotation mark.
; Thus, (quote a) is same as 'a and (quote (a b c)) is same
; as '(a b c).
; The quotation mark is a single-character abbrevation for
; wrapping the next complete expression with quote to form
; (quote <expression>). This is important because it
; maintains the principle that any expression seen by the
; interpreter can be manipulated as a data object. For
; instance the expression (car '(a b c)), which is same as
; (car (quote (a b c))), can be constructed by evaluating:
(list 'car (list 'quote '(a b c)))
; or
'(car (quote (a b c)))

; eq? tests whether two symbols are same or not. By same we
; mean having same characters in the same order.
(eq? 'a 'a)
(eq? '(a b c) '(a b d))

; 'eq?' cannot compare lists like 'equal?' (from exercise 2.54):
(define a-list '(a b c))

(eq? a-list a-list)
(eq? a-list '(a b c))
(eq? '(a b c) '(a b c))

; Using eq?, we can implement a useful procedure called
; memq. It takes two arguments, a symbol and a list. If
; the symbol is not contained in the list, then memq
; returns false. Otherwise, it returns the sublist of the
; beginning with the first occurence of the symbol:
(define (memq item x)
  (cond ((null? x) false)
        ((eq? item (car x)) x)
        (else (memq item (cdr x)))))

(memq 'apple '(pear banana prune))
(memq 'apple '(x (apple sauce) y apple pear))
