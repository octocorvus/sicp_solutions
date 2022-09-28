#lang sicp

(define (make-leaf symbol weight)
  (list 'leaf symbol weight))

(define (leaf? object)
  (eq? (car object) 'leaf))

(define (symbol-leaf x) (cadr x))

(define (weight-leaf x) (caddr x))

(define (make-code-tree left right)
  (list left
        right
        (append (symbols left)
                (symbols right))
        (+ (weight left) (weight right))))

(define (left-branch tree) (car tree))

(define (right-branch tree) (cadr tree))

(define (symbols tree)
  (if (leaf? tree)
      (list (symbol-leaf tree))
      (caddr tree)))

(define (weight tree)
  (if (leaf? tree)
      (weight-leaf tree)
      (cadddr tree)))

(define (decode bits tree)
  (define (decode-1 bits current-branch)
    (if (null? bits)
        '()
        (let ((next-branch
               (choose-branch
                (car bits)
                current-branch)))
          (if (leaf? next-branch)
              (cons
               (symbol-leaf next-branch)
               (decode-1 (cdr bits) tree))
              (decode-1 (cdr bits)
                        next-branch)))))
  (decode-1 bits tree))

(define (choose-branch bit branch)
  (cond ((= bit 0) (left-branch branch))
        ((= bit 1) (right-branch branch))
        (else (error "bad bit: CHOOSE-BRANCH" bit))))

(define (adjoin-set x set)
  (cond ((null? set) (list x))
        ((< (weight x) (weight (car set)))
         (cons x set))
        (else
         (cons (car set)
               (adjoin-set x (cdr set))))))

(define (make-leaf-set pairs)
  (if (null? pairs)
      '()
      (let ((pair (car pairs)))
        (adjoin-set
         (make-leaf (car pair)    ; symbol
                    (cadr pair))  ; frequency
         (make-leaf-set (cdr pairs))))))

(define (encode message tree)
  (if (null? message)
      '()
      (append
       (encode-symbol (car message)
                      tree)
       (encode (cdr message) tree))))

(define (encode-symbol symbol tree)
  (cond ((leaf? tree) '())
        ((element-of-set?
          symbol
          (symbols (left-branch tree)))
         (cons 0
               (encode-symbol symbol
                              (left-branch tree))))
        ((element-of-set?
          symbol
          (symbols (right-branch tree)))
         (cons 1
               (encode-symbol symbol
                              (right-branch tree))))
        (else (error "bad symbol: ENCODE-SYMBOL"
                     symbol))))

(define (element-of-set? x set)
  (cond ((null? set) false)
        ((equal? x (car set)) true)
        (else (element-of-set? x (cdr set)))))

(define (generate-huffman-tree pairs)
  (successive-merge
   (make-leaf-set pairs)))

(define (successive-merge nodes)
  (if (null? (cdr nodes))
      (car nodes)
      (successive-merge
       (adjoin-set
        (make-code-tree
         (car nodes)
         (cadr nodes))
        (cddr nodes)))))

(define sample-tree
  (generate-huffman-tree
   '((A 4) (B 2) (C 1) (D 1))))

(define sample-message
  '(0 1 1 0 0 1 0 1 0 1 1 1 0))

(decode sample-message sample-tree)

(define encoded-message
  (encode '(A D A B B C A) sample-tree))

(equal? sample-message encoded-message)

(define rock-tree
  (generate-huffman-tree
   '((A 2)
     (BOOM 1)
     (GET 2)
     (JOB 2)
     (NA 16)
     (SHA 3)
     (YIP 9)
     (WAH 1))))

(define encoded-lyrics
  (encode
   '(GET A JOB
     SHA NA NA NA NA NA NA NA NA

     GET A JOB
     SHA NA NA NA NA NA NA NA NA

     WAH YIP YIP YIP YIP
     YIP YIP YIP YIP YIP
     SHA BOOM) rock-tree))

(display encoded-lyrics)
(newline)
(display "Number of bits: ")
(display (length encoded-lyrics))
(newline)

(define decoded-lyrics
  (decode encoded-lyrics rock-tree))

(display decoded-lyrics)
(newline)
(display "Number of symbols in message: ")
(display (length decoded-lyrics))
(newline)
(display "Bits required by fixed-length code: ")
(display (* 3 (length decoded-lyrics)))
(newline)
