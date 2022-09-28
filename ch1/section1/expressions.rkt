#lang sicp

; A primitive expression, the interpreter will respond by printing 486.
486

; Expressions may be combined to form compound expressions.
(+ 137 349)
(- 1000 334)
(* 5 99)
(/ 10 5)
(+ 2.7 10)
; Expressions such as these, formed by delimiting a list of expressions
; within parentheses in order to denote a procedure application, are
; called combinations. The leftmost element in the list is called the
; operator, and the other elements are called operands. The value of a
; combination is obtained by applying the procedure specified by the
; operator to the arguments that are the values of the opearands.

; The convention of placing the operator to the left of the operand is
; known as prefix notation. It may be confusing at first. However, it
; has several advantages.
; * It can accommodate procedures that may take arbitrary number of
;   arguments:
;   > (+ 21 35 12 7)
;   75
;   > (* 25 4 12)
;   1200
;   No ambiguity can arise here because the operator is always on the
;   left and the whole expressions is delimited by the parentheses.
; * It extends in a straightforward way to allow combinations to be
;   nested.
;   > (+ (* 3 5) (- 10 6))
;   19
;   > (+ (* 3 (+ (* 2 4) (+ 3 5))) (+ (- 10 7) 6))
;   57
;   This is kinda confusing, so we can help ourselves by writing it
;   like, following a formatting convention known as pretty-printing,
;   in which each long combination is written so that the operands are
;   aligned vertically.
;   > (+ (* 3
;           (+ (* 2 4)
;              (+ 3 5)))
;        (+ (- 10 7)
;           6))
(+ (* 3
      (+ (* 2 4)
         (+ 3 5)))
   (+ (- 10 7)
      6))

(* (+ 2 (* 4 6))
   (+ 3 5 7))
