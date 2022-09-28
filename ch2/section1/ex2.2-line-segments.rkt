#lang sicp

; Line segment
(define (make-segment start end)
  (cons start end))
(define (start-segment line-segment)
  (car line-segment))
(define (end-segment line-segment)
  (cdr line-segment))

; Point
(define (make-point x y)
  (cons x y))
(define (x-point p)
  (car p))
(define (y-point p)
  (cdr p))

(define (print-point p)
  (newline)
  (display "(")
  (display (x-point p))
  (display ",")
  (display (y-point p))
  (display ")"))

(define (average x y)
  (/ (+ x y) 2))

(define (midpoint-segment line-segment)
  (make-point
   (average
    (x-point (start-segment line-segment))
    (x-point (end-segment line-segment)))
   (average
    (y-point (start-segment line-segment))
    (y-point (end-segment line-segment)))))

(define line-segment1
  (make-segment (make-point 2 8)
                (make-point -3 78)))

(print-point
 (midpoint-segment line-segment1))

(define line-segment2
  (make-segment (make-point 0 2)
                (make-point 2 0)))

(print-point
 (midpoint-segment line-segment2))
