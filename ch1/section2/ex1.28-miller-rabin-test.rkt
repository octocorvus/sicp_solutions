#lang sicp

(define (square x)
  (* x x))

(define (expmod base exp m)
  (define (iter result base exp)
    (cond ((= exp 0)
           result)
          ((even? exp)
           (let ((root (remainder (* result
                                     (square base))
                                  m)))
             (if (and (not (or (= root 1)
                               (= root
                                  (- m 1))))
                      (= (remainder (square root)
                                    m)
                         1))
                 0
                 (iter result
                       (remainder (square base)
                                  m)
                       (/ exp 2)))))
          (else
           (iter (remainder (* result base)
                            m)
                 base
                 (- exp 1)))))
  (iter 1 base exp))

(define (miller-rabin-test n)
  (define (try-it a)
    (define expmod-var (expmod a n n))
    (if (= expmod-var 0)
        false
        (= expmod-var a)))
  (try-it (+ 1 (random (- n 1)))))

(define (fast-prime? n times)
  (cond ((= times 0) true)
        ((miller-rabin-test n)
         (fast-prime? n (- times 1)))
        (else false)))

(define (test-prime start end step)
  (define (iter counter)
    (newline)
    (display "Testing ")
    (display counter)
    (if (fast-prime? counter 100)
        (display " ** Prime")
        (display " ** Not a Prime"))
    (if (< counter end)
        (iter (+ counter step))))
  (iter start))

;(test-prime 3 10000 2)

(define (test-prime-new start end)
  (cond ((< start end)
         (cond ((fast-prime? start 100)
                (display start)
                (newline)))
         (test-prime-new (+ start 1) end))))

(test-prime-new 2 100000)


