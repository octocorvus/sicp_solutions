#lang sicp

; We represent a directed line segment in a plane
; as a pair of vectors -- the vector running from
; the origin to the start-point of the segment,
; and the vector running from the origin to the
; end-point of the segment.
(define (make-segment start-vect end-vect)
  (list start-vect end-vect))

(define (start-segment segment)
  (list-ref segment 0))

(define (end-segment segment)
  (list-ref segment 1))

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
