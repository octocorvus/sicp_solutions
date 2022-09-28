#lang racket

(require berkeley)

(define (make-interval a b) (cons a b))
(define (lower-bound interval) (car interval))
(define (upper-bound interval) (cdr interval))

(define (sub-interval i1 i2)
  (make-interval
   (- (lower-bound i1)
      (upper-bound i2))
   (- (upper-bound i1)
      (lower-bound i2))))

(define (width interval)
  (/ (- (upper-bound interval)
        (lower-bound interval))
     2.0))

(define (add-interval i1 i2)
  (make-interval
   (+ (lower-bound i1)
      (lower-bound i2))
   (+ (upper-bound i1)
      (upper-bound i2))))

(define (mul-interval i1 i2)
  (let* ((lb1 (lower-bound i1))
         (ub1 (upper-bound i1))
         (lb2 (lower-bound i2))
         (ub2 (upper-bound i2))
         (bounds (se (* lb1 lb2)
                     (* lb1 ub2)
                     (* ub1 lb2)
                     (* ub1 ub2))))
    (make-interval
     (apply min bounds)
     (apply max bounds))))

(define (reciprocal-interval interval)
  (if (spans-zero-interval? interval)
      (error "RECIPROCAL-INTERVAL: interval spans zero" interval)
      (make-interval
       (/ (upper-bound interval))
       (/ (lower-bound interval)))))

(define (spans-zero-interval? interval)
  (not (positive? (* (lower-bound interval)
                     (upper-bound interval)))))

(define (div-interval i1 i2)
  (mul-interval
   i1
   (reciprocal-interval i2)))

(define (make-center-percent center percent)
  (let ((width (* center (/ percent 100))))
    (make-interval
     (- center width)
     (+ center width))))

(define (center interval)
  (average (lower-bound interval)
           (upper-bound interval)))

(define (percent interval)
  (* (/ (width interval)
        (center interval))
     100.0))

(define (average . numbers)
  (/ (apply sum numbers)
     (length numbers)))

(define (sum . numbers)
  (accumulate + 0 numbers))
