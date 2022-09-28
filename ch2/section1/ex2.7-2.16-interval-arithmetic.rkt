#lang sicp

(define (add-interval x y)
  (make-interval (+ (lower-bound x)
                    (lower-bound y))
                 (+ (upper-bound x)
                    (upper-bound y))))

(define (sub-interval x y)
  (make-interval (- (lower-bound x)
                    (upper-bound y))
                 (- (upper-bound x)
                    (lower-bound y))))

(define (old-mul-interval x y)
  (let ((p1 (* (lower-bound x)
               (lower-bound y)))
        (p2 (* (lower-bound x)
               (upper-bound y)))
        (p3 (* (upper-bound x)
               (lower-bound y)))
        (p4 (* (upper-bound x)
               (upper-bound y))))
    (make-interval (min p1 p2 p3 p4)
                   (max p1 p2 p3 p4))))

(define (mul-interval x y)
  (define (positive-pair? x y)
    (and (positive? x)
         (positive? y)))
  (define (negative-pair? x y)
    (and (negative? x)
         (negative? y)))
  (let ((xmin (lower-bound x))
        (xmax (upper-bound x))
        (ymin (lower-bound y))
        (ymax (upper-bound y)))
    (cond ((positive-pair? xmin xmax)
           (cond ((positive-pair? ymin ymax)
                  (make-interval (* xmin ymin) (* xmax ymax)))
                 ((negative-pair? ymin ymax)
                  (make-interval (* xmax ymin) (* xmin ymax)))
                 (else
                  (make-interval (* xmax ymin) (* xmax ymax)))))
          ((negative-pair? xmin xmax)
           (cond ((positive-pair? ymin ymax)
                  (make-interval (* xmin ymax) (* xmax ymin)))
                 ((negative-pair? ymin ymax)
                  (make-interval (* xmax ymax) (* xmin ymin)))
                 (else
                  (make-interval (* xmin ymax) (* xmin ymin)))))
          (else
           (cond ((positive-pair? ymin ymax)
                  (make-interval (* xmin ymax) (* xmax ymax)))
                 ((negative-pair? ymin ymax)
                  (make-interval (* xmax ymin) (* xmin ymin)))
                 (else
                  (make-interval (min (* xmin ymax) (* xmax ymin))
                                 (max (* xmin ymin) (* xmax ymax)))))))))

(define (div-interval x y)
  (if (negative? (* (lower-bound y)
                    (upper-bound y)))
      (error "Division by interval spanning zero"
             y)
      (mul-interval x
                    (make-interval
                     (/ 1.0 (upper-bound y))
                     (/ 1.0 (lower-bound y))))))

(define (make-interval a b)
  (if (> a b)
      (error "Lower bound greater than upper bound")
      (cons a b)))

(define (make-center-width c w)
  (make-interval (- c w) (+ c w)))

(define (make-center-percent c p)
  (make-center-width c (/ (* c p) 100)))

(define (lower-bound x)
  (car x))

(define (upper-bound x)
  (cdr x))

(define (center i)
  (/ (+ (lower-bound i)
        (upper-bound i))
     2))

(define (width i)
  (/ (- (upper-bound i)
        (lower-bound i))
     2))

(define (percent i)
  (* (/ (width i) (center i)) 100))

(width
 (mul-interval (make-interval 1.9 2.1)
               (make-interval 2.8 3.2)))

(width
 (div-interval (make-interval -1.3 -0.7)
               (make-interval 6.5 7.5)))

(width
 (add-interval (make-interval 1.9 2.1)
               (make-interval 2.8 3.2)))

(width
 (sub-interval (make-interval 1.9 2.1)
               (make-interval 2.8 3.2)))

; Test cases for mul-interval
(define (print-interval x)
  (display "[")
  (display (lower-bound x))
  (display ",")
  (display (upper-bound x))
  (display "]")
  (newline))

(define (equal-interval? x y)
  (define (close-enough? x y)
    (< (abs (- x y)) 0.00001))
  (and (close-enough? (lower-bound x)
                      (lower-bound y))
       (close-enough? (upper-bound x)
                      (upper-bound y))))

(define (random-interval range)
  (let ((x (lower-bound range))
        (y (upper-bound range)))
    (let ((x (+ x (random (- y x 0.0))))
          (y (+ x (random (- y x 0.0)))))
      (make-interval (min x y) (max x y)))))

(define (test-mul-interval n range)
  (cond ((positive? n)
         (let ((x (random-interval range))
               (y (random-interval range)))
           (let ((a (mul-interval x y))
                 (b (old-mul-interval x y)))
             (cond ((equal-interval? a b)
                    (test-mul-interval (- n 1) range))
                   (else
                    (display "mul-interval test failed at:")
                    (newline)
                    (display "  x : ")
                    (print-interval x)
                    (display "  y : ")
                    (print-interval y)
                    (display "  (mul-interval x y) : ")
                    (print-interval a)
                    (display "  (old-mul-interval x y) : ")
                    (print-interval b))))))
        (else (display "All tests passed!")
              (newline))))
