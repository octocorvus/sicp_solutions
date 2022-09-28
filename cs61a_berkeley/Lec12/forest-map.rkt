#lang racket

;;; Trees, version 1
;;;
;;; Data at all nodes, no empty tree defined.

(define (make-tree datum children)
  (cons datum children))
(define (datum node)
  (car node))
(define (children node)
  (cdr node))

(define (leaf? node)
  (empty? (children node)))

(define (leaves . seq)
  (map (lambda (x) (make-tree x '())) seq))

(define t1
  (make-tree 1
             (list (make-tree 2 (leaves 3 4))
                   (make-tree 5 (leaves 6 7 8)))))

(define (treemap fn tree)
  (make-tree (fn (datum tree))
             (forest-map fn (children tree))))

(define (forest-map fn forest)
  (if (empty? forest)
      '()
      (cons (treemap fn (car forest))
            (forest-map fn (cdr forest)))))

(define (accumulate proc null-value sequence)
  (if (null? sequence)
      null-value
      (proc (car sequence)
            (accumulate proc
                        null-value
                        (cdr sequence)))))

(define (data tree)
  (cons (datum tree)
        (accumulate append
                    '()
                    (map data (children tree)))))

;; But can we implement data using treemap?
;; I guess not with my current knowledge of scheme.
