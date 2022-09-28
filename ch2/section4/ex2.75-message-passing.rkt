#lang sicp

(define (square x) (* x x))

(define (make-from-real-imag x y)
  (define (dispatch op)
    (cond ((eq? op 'real-part) x)
          ((eq? op 'imag-part) y)
          ((eq? op 'magnitude)
           (sqrt (+ (square x) (square y))))
          ((eq? op 'angle) (atan y x))
          (else
           (error "Unknown op: 
            MAKE-FROM-REAL-IMAG" op))))
  dispatch)

(define (make-from-mag-ang r a)
  (define (dispatch op)
    (cond ((eq? op 'real-part)
           (* r (cos a)))
          ((eq? op 'imag-part)
           (* r (sin a)))
          ((eq? op 'magnitude) r)
          ((eq? op 'angle) a)
          (else
           (error "Unknown op:
          MAKE-FROM-MAG-AND" op))))
  dispatch)

(define (apply-generic op arg) (arg op))

(define (real-part z)
  (apply-generic 'real-part z))
(define (imag-part z)
  (apply-generic 'imag-part z))
(define (magnitude z)
  (apply-generic 'magnitude z))
(define (angle z)
  (apply-generic 'angle z))

(define (add-complex z1 z2)
  (make-from-real-imag
   (+ (real-part z1) (real-part z2))
   (+ (imag-part z1) (imag-part z2))))

(define (sub-complex z1 z2)
  (make-from-real-imag
   (- (real-part z1) (real-part z2))
   (- (imag-part z1) (imag-part z2))))

(define (mul-complex z1 z2)
  (make-from-mag-ang
   (* (magnitude z1) (magnitude z2))
   (+ (angle z1) (angle z2))))

(define (div-complex z1 z2)
  (make-from-mag-ang
   (/ (magnitude z1) (magnitude z2))
   (- (angle z1) (angle z2))))

(define z1 (make-from-real-imag 1 2))
(define z2 (make-from-mag-ang 3 4))

(define (print-complex z)
  (display "real-part: ")
  (display (real-part z))
  (newline)
  (display "imag-part: ")
  (display (imag-part z))
  (newline)
  (display "magnitude: ")
  (display (magnitude z))
  (newline)
  (display "angle: ")
  (display (angle z))
  (newline)
  (display '---)
  (newline))

(print-complex z1)
(print-complex z2)

(print-complex (add-complex z1 z2))
(print-complex (mul-complex z1 z2))
(print-complex (sub-complex z1 z2))
(print-complex (div-complex z1 z2))

(print-complex (add-complex z2 z1))
(print-complex (mul-complex z2 z1))
(print-complex (sub-complex z2 z1))
(print-complex (div-complex z2 z1))
