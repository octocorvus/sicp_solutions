#lang sicp

; Ex2.46: Vector operations.
; A two-dimensional vector v running from the origin
; to a point can be represented as a pair consisting
; of an x-coordinate and y-coordinate. Implement a
; data abstraction for vectors by giving a constructor
; make-vect and corresponding selectors xcor-vect and
; ycor-vect. In terms of your selectors and
; constructors implement procedures add-vect, sub-vect,
; and scale-vect that performs the operations vector
; addition, vector subtraction, and multiplying a
; vector by a scalar.

(define (make-vect x y)
  (list x y))

(define (xcor-vect v)
  (list-ref v 0))

(define (ycor-vect v)
  (list-ref v 1))

; Add two vectors.
; (x1,y1)+(x2,y2)=(x1+x2,y1+y2).
(define (add-vect v1 v2)
  (make-vect (+ (xcor-vect v1)
                (xcor-vect v2))
             (+ (ycor-vect v1)
                (ycor-vect v2))))

; Subtract two vectors.
; (x1,y1)+(x2,y2)=(x1-x2,y1-y2)
(define (sub-vect v1 v2)
  (add-vect v1 (additive-inverse v2)))

; Additive inverse of a vector.
; AdditiveInverse((x,y))=(-x,-y)
(define (additive-inverse v)
  (make-vect (- (xcor-vect v))
             (- (ycor-vect v))))

; Scale a vector.
; s*(x,y)=(s*x,s*y)
(define (scale-vect s v)
  (make-vect (* s (xcor-vect v))
             (* s (ycor-vect v))))
