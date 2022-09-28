#lang sicp

; Let us begin by assuming that we already have a way of constructing a
; rectangle and a way of extracting the width and height. Let us further
; assume that constructors and selectors are available as procedures:
; (make-rect <formal parameters>)
; (width-rect <rect>)
; (height-rect <rect>)
; If we have these procedures, we can calculate area and perimeter like
; this:
; area = width x height
; perimeter = 2 x (width + height)
; We can express these rules using procedures:

(define (area-rect rect)
  (* (width-rect rect)
     (height-rect rect)))

(define (perimeter-rect rect)
  (* 2 (+ (width-rect rect)
          (height-rect rect))))

(define (print-rect rect)
  (display "Rectangle:\n")
  (display "  Width: ")
  (display (width-rect rect))
  (newline)
  (display "  Height: ")
  (display (height-rect rect))
  (newline)
  (display "  Origin: ")
  (print-point (origin-rect rect))
  (newline)
  (display "  Rotation: ")
  (display (rot-rect rect))
  (newline)
  (display "  Area: ")
  (display (area-rect rect))
  (newline)
  (display "  Perimeter: ")
  (display (perimeter-rect rect))
  (newline))

; Now we need the constructor make-rect and selectors width-rect and
; height-rect.
; Our second implementation, just like the first, takes width and height of
; rectangle along with one translational (a point) and one rotational
; parameter (angle, in degrees). Here rectangle is represented internally as
; a quadrilateral.

(define (make-rect
         width height origin rot)
  (cons rot
        (move-quad
         (rotate-quad
          (make-quad (make-point 0 0)
                     (make-point width 0)
                     (make-point width height)
                     (make-point 0 height))
          rot)
         (make-point (- (x-point origin))
                     (- (y-point origin))))))

(define (coordinates-rect rect)
  (cdr rect))

; Vertex-rect is another abstraction boundary since we didn't need to change
; anything that dependent on vertex-rect after we changed make-rect to
; include angle of rotation directly.
(define (vertex-rect rect index)
  (vertex-quad (coordinates-rect rect) index))

(define (width-rect rect)
  (distance-point (vertex-rect rect 0)
                  (vertex-rect rect 1)))

(define (height-rect rect)
  (distance-point (vertex-rect rect 1)
                  (vertex-rect rect 2)))

(define (origin-rect rect)
  (vertex-rect rect 0))

(define (rot-rect rect)
  (car rect))

; A quadrilateral, made up of four points. Suppose we can get coordinates of
; the vertices of the quadrilateral by (vertex quad index).

(define (transform-quad quad transform)
  (make-quad (transform-point
              (vertex-quad quad 0)
              transform)
             (transform-point
              (vertex-quad quad 1)
              transform)
             (transform-point
              (vertex-quad quad 2)
              transform)
             (transform-point
              (vertex-quad quad 3)
              transform)))

; Rotate the quadrilateral about origin.
(define (rotate-quad quad angle)
  (transform-quad quad (rotational-transform angle)))

; Move the quadrilateral.
(define (move-quad quad origin)
  (transform-quad quad (linear-transform origin)))

; The quadrilateral.
(define (make-quad p1 p2 p3 p4)
  (cons (cons p1 p2)
        (cons p3 p4)))

(define (vertex-quad quad index)
  (cond ((= index 0) (car (car quad)))
        ((= index 1) (cdr (car quad)))
        ((= index 2) (car (cdr quad)))
        ((= index 3) (cdr (cdr quad)))
        (else (error "Invalid index"))))

; Point
(define (make-point x y)
  (cons x y))
(define (x-point p)
  (car p))
(define (y-point p)
  (cdr p))

(define (print-point p)
  (display "(")
  (display (x-point p))
  (display ",")
  (display (y-point p))
  (display ")"))

; Procedures that manipulate points

(define (square x) (* x x))

(define (distance-point p1 p2)
  (sqrt (+ (square (- (x-point p1)
                      (x-point p2)))
           (square (- (y-point p1)
                      (y-point p2))))))

(define (transform-point p transform)
  (transform p))

(define (linear-transform origin)
  (lambda (p)
    (make-point (- (x-point p) (x-point origin))
                (- (y-point p) (y-point origin)))))

(define (rotational-transform angle)
  (lambda (p)
    (let ((angle (* angle (/ pi 180)))
          (x (x-point p))
          (y (y-point p)))
      (make-point (- (* x (cos angle))
                     (* y (sin angle)))
                  (+ (* x (sin angle))
                     (* y (cos angle)))))))

(define pi (* 4 (atan 1)))

; Program that uses rectangle
(define rect
  (make-rect 69
             420
             (make-point 69 420)
             180))

(print-rect rect)
