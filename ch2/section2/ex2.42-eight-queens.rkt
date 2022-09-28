#lang sicp

; Eight queens puzzle: We want to place eight queens on
; on a chessboard such that no queen is in check from any
; other.
; One way to solve the puzzle is to work across the board,
; placing a queen in each column. Once we have placed k-1
; queens, we must place kth queen in a position where it
; does not check nay of the queens already on the board.
; We can formulate this approach recursively: Assume we
; have already generated the sequence of all possible ways
; to place k-1 queens in the first k-1 columns of the
; board. For each of these ways, generate an extended set
; of postions by placing a queen in each row of the kth
; column. Now filter these, keeping only the positions for
; which the queen in the kth column is safe w.r.t. to the
; other queens. This procedure will produce the sequence
; of all ways to place k queens in the first k column.
; Procedure queens returns a sequence of all solutions to
; the problem of placing n queens of an n by n chessboard.
(define (queens board-size)
  ; Return all possible ways to place k queens in first k
  ; columns of the board such that all queens are safe.
  (define (queen-cols k)
    (if (= k 0)
        (list empty-board)
        ; We filter out the position where the kth queen
        ; is safe.
        (filter
         (lambda (positions)
           (safe? k positions))
         ; All ways to place kth queen on a board with k-1
         ; queens already placed.
         (flatmap
          ; Rest-of-queens is a way to place k-1 queens in
          ; the first k-1 columns.
          (lambda (rest-of-queens)
            ; New-row is a proposed row in which to place
            ; the queen for the kth column. Essentially it
            ; is the row number. Thus, we're placing the
            ; kth queen in kth column and (new-row)th row.
            (map (lambda (new-row)
                   ; We adjoin new row-column position to
                   ; rest-of-queens.
                   (adjoin-position
                    new-row
                    k
                    rest-of-queens))
                 ; We enumerate the row number.
                 (enumerate-interval
                  1
                  board-size)))
          ; All ways to place k-1 queens safely in first
          ; k-1 columns. We expand this by placing kth
          ; queen in kth column.
          (queen-cols (- k 1))))))
  (queen-cols board-size))

; Create a point.
(define (make-point x y)
  (list x y))

; Get x-coordinate of a point.
(define (x-point p)
  (list-ref p 0))

; Get y-coordinate of a point.
(define (y-point p)
  (list-ref p 1))

; Empty board containing no queens.
(define empty-board nil)

; Check if two points are in the same row.
(define (same-row? p1 p2)
  (= (x-point p1) (x-point p2)))

; Check if two points are in the same column.
(define (same-column? p1 p2)
  (= (y-point p1) (y-point p2)))

; Check if two points are in the same diagonal.
(define (same-diag? p1 p2)
  (= (abs (- (x-point p1)
             (x-point p2)))
     (abs (- (y-point p1)
             (y-point p2)))))

; Check if two queens are in check with each other given
; their positions.
(define (queen-check? p1 p2)
  (or (same-row? p1 p2)
      (same-column? p1 p2)
      (same-diag? p1 p2)))

; Push an element to the end of the list.
(define (push elem sequence)
  (append sequence (list elem)))

; Adjoin a new row-column position to a set of positions.
(define (adjoin-position
         row col positions)
  (push (make-point row col)
        positions))

; Remove all occurences of an elemnet from a list.
(define (remove elem sequence)
  (filter (lambda (x)
            (not (equal? x elem)))
          sequence))

; Check if the queen in kth column is safe w.r.t. other
; queens.
(define (safe? k positions)
  ; Check if a queen is safe w.r.t. other queens.
  (define (safe-queen? queen other-queens)
    ; If there are no other queens the queen is safe.
    ; Otherwise, the queen is safe if and only if it is
    ; not in check with first queen and is safe w.r.t.
    ; rest of the queens.
    (if (null? other-queens)
        true
        (and (not
              (queen-check?
               queen (car other-queens)))
             (safe-queen?
              queen (cdr other-queens)))))
  ; The queen is only safe if and only if it is safe
  ; w.r.t. queens other than itself. So, we remove the
  ; queen from positions and check whether it safe or not.
  (let ((queen (list-ref positions (- k 1))))
    (safe-queen?
     queen (remove queen positions))))

; Filter elements of a sequence using a given predicate.
(define (filter predicate sequence)
  (cond ((null? sequence) nil)
        ((predicate (car sequence))
         (cons (car sequence)
               (filter predicate
                       (cdr sequence))))
        (else (filter predicate
                      (cdr sequence)))))

; Applies proc to each element of seq and append the
; results.
(define (flatmap proc sequence)
  (accumulate append nil (map proc sequence)))

; Combines elements of the sequence using 'op' given
; a null value 'initial'.
(define (accumulate op initial sequence)
  (if (null? sequence)
      initial
      (op (car sequence)
          (accumulate op
                      initial
                      (cdr sequence)))))

; Return list of all numbers between low and high,
; both inclusive.
(define (enumerate-interval low high)
  (if (> low high)
      nil
      (cons low
            (enumerate-interval
             (+ low 1)
             high))))

(define (test-queens n)
  (define (iter count)
    (cond ((not (> count n))
           (display "Board size: ")
           (display count)
           (newline)
           (let ((t0 (runtime)))
             (queens count)
             (let ((t1 (- (runtime) t0)))
               (display "Time taken: ")
               (display t1)
               (newline)))
           (iter (+ count 1)))))
  (iter 1))

(test-queens 8)

(define (execution-time proc args)
  (let ((t0 (runtime)))
    (apply proc args)
    (- (runtime) t0)))

(define (test proc test-cases)
  (define (iter total-time count proc test-cases)
    (if (null? test-cases)
        (/ total-time count)
        (iter (+ total-time
                 (execution-time
                  proc
                  (car test-cases)))
              (+ count 1)
              proc
              (cdr test-cases))))
  (if (null? test-cases)
      (error "No test case")
      (iter 0.0 0 proc test-cases)))

; n is list of elements in the sequence.
; num-test-cases is number of test cases.
(define (test-safe n num-test-cases)
  (define (random-num n)
    (+ (random n) 1))
  ; Generate random sequence of n positive integers.
  (define (random-sequence n)
    (define (iter count sequence)
      (if (= count n)
          sequence
          (iter (+ count 1)
                (cons (random-num 1000)
                      sequence))))
    (iter 0 nil))
  ; Generate sequence of random points
  (define (random-points n)
    (map (lambda (y)
           (make-point
            (random-num 1000) y))
         (random-sequence n)))
  ; Generate test cases.
  (define (generate-test-cases num-test-cases)
    (define (iter test-cases count)
      (if (= count num-test-cases)
          test-cases
          (iter (cons (random-points n)
                      test-cases)
                (+ count 1))))
    (map (lambda (seq)
           (list n seq))
         (iter nil 0)))
  (test safe? (generate-test-cases num-test-cases)))

; Profiling for queens procedure
(execution-time queens (list 1))
(execution-time queens (list 2))
(execution-time queens (list 3))
(execution-time queens (list 4))
(execution-time queens (list 5))
(execution-time queens (list 6))
(execution-time queens (list 7))
(execution-time queens (list 8))
