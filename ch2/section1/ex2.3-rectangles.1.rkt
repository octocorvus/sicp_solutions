#lang sicp

; Let us begin by assuming that we already have a way of constructing a
; rectangle and a way of extracting the width and height. Let us further
; assume that constructors and selectors are available as procedures:
; (make-rect <formal parameters>)
; (width-rect <rect>)
; (height-rect <rect>)
; If we have these procedures, we can calculate area and perimeter like this:
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
; Our first implementation takes width and height of rectangle along with
; one translational (a point) and one rotational parameter (angle, in
; degrees). Rectangle is represented internally as:
; ( ( width . height ) . ( origin . rot ) )

(define (make-rect
         width height origin rot)
  (cons (cons width height)
        (cons origin rot)))

(define (dimensions-rect rect)
  (car rect))

(define (width-rect rect)
  (car (dimensions-rect rect)))

(define (height-rect rect)
  (cdr (dimensions-rect rect)))

(define (origin-rect rect)
  (car (cdr rect)))

(define (rot-rect rect)
  (cdr (cdr rect)))

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

; Program that uses rectangle
(define rect
  (make-rect 69
             420
             (make-point 69 420)
             180))

(print-rect rect)
