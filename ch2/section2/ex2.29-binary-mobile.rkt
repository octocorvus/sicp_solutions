#lang sicp

(define (make-mobile left right)
  (cons left right))

(define (make-branch length structure)
  (cons length structure))

(define (left-branch mobile)
  (car mobile))

(define (right-branch mobile)
  (cdr mobile))

(define (branch-length branch)
  (car branch))

(define (branch-structure branch)
  (cdr branch))

; Return total weight of a mobile.
(define (total-weight mobile)
  (if (number? mobile)
      mobile
      (+ (total-weight
          (branch-structure
           (left-branch mobile)))
         (total-weight
          (branch-structure
           (right-branch mobile))))))

(define (torque-branch branch)
  (* (branch-length branch)
     (total-weight (branch-structure branch))))

; Test whether a mobile is balanced or not.
(define (balanced-mobile? mobile)
  (if (number? mobile)
      true
      (let ((lb (left-branch mobile))
            (rb (right-branch mobile)))
        (and (= (torque-branch lb)
                (torque-branch rb))
             (balanced-mobile? (branch-structure lb))
             (balanced-mobile? (branch-structure rb))))))

(define x (make-mobile (make-branch 10 20)
                       (make-branch 20 (make-mobile
                                        (make-branch 7 4)
                                        (make-branch 8 10)))))

(total-weight x)
(balanced-mobile? x)

(define y
  (make-mobile
   (make-branch
    4
    (make-mobile
     (make-branch 5 21)
     (make-branch 7 15)))
   (make-branch
    6
    24)))

(total-weight y)
(balanced-mobile? y)
