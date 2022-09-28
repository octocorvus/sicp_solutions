#lang sicp

; Procedure
(define (gcd a b)
  (if (= b 0)
      a
      (gcd b (remainder a b))))

; Normal order evaluation of (gcd 206 40)
;; 1: 0
(gcd 206 40)
;; 2: 0
(if (= 40 0) ; false
    206
    (gcd 40 (remainder 206 40)))
;; 3: 0
(gcd 40 (remainder 206 40))
;; 4: 1
(if (= (remainder 206 40) 0) ; 1 remainder operation performed
    40
    (gcd (remainder 206 40)
         (remainder 40
                    (remainder 206 40))))
(if (= 6 0)
    40
    (gcd (remainder 206 40)
         (remainder 40
                    (remainder 206 40))))
;; 5: 1
(gcd (remainder 206 40)
     (remainder 40
                (remainder 206 40)))
;; 6: 3
(if (= (remainder 40
                  (remainder 206 40))
       0) ; 2 remainder operations performed
    (remainder 206 40)
    (gcd (remainder 40
                    (remainder 206 40))
         (remainder (remainder 206 40)
                    (remainder 40
                               (remainder 206 40)))))
(if (= (remainder 40 6) 0)
    (remainder 206 40)
    (gcd (remainder 40
                    (remainder 206 40))
         (remainder (remainder 206 40)
                    (remainder 40
                               (remainder 206 40)))))
(if (= 4 0) ; false
    (remainder 206 40)
    (gcd (remainder 40
                    (remainder 206 40))
         (remainder (remainder 206 40)
                    (remainder 40
                               (remainder 206 40)))))
;; 7: 3
(gcd (remainder 40
                (remainder 206 40))
     (remainder (remainder 206 40)
                (remainder 40
                           (remainder 206 40))))
;; 8: 7
(if (= (remainder (remainder 206 40)
                  (remainder 40
                             (remainder 206 40))) 0) ; 4 reaminder operations performed
    (remainder 40
               (remainder 206 40))
    (gcd (remainder (remainder 206 40)
                    (remainder 40
                               (remainder 206 40)))
         (remainder (remainder 40
                               (remainder 206 40))
                    (remainder (remainder 206 40)
                               (remainder 40
                                          (remainder 206 40))))))
(if (= (remainder 6
                  (remainder 40
                             (remainder 206 40))) 0)
    (remainder 40
               (remainder 206 40))
    (gcd (remainder (remainder 206 40)
                    (remainder 40
                               (remainder 206 40)))
         (remainder (remainder 40
                               (remainder 206 40))
                    (remainder (remainder 206 40)
                               (remainder 40
                                          (remainder 206 40))))))
(if (= (remainder 6
                  (remainder 40
                             6)) 0)
    (remainder 40
               (remainder 206 40))
    (gcd (remainder (remainder 206 40)
                    (remainder 40
                               (remainder 206 40)))
         (remainder (remainder 40
                               (remainder 206 40))
                    (remainder (remainder 206 40)
                               (remainder 40
                                          (remainder 206 40))))))
(if (= (remainder 6
                  4) 0)
    (remainder 40
               (remainder 206 40))
    (gcd (remainder (remainder 206 40)
                    (remainder 40
                               (remainder 206 40)))
         (remainder (remainder 40
                               (remainder 206 40))
                    (remainder (remainder 206 40)
                               (remainder 40
                                          (remainder 206 40))))))
(if (= 2 0) ; false
    (remainder 40
               (remainder 206 40))
    (gcd (remainder (remainder 206 40)
                    (remainder 40
                               (remainder 206 40)))
         (remainder (remainder 40
                               (remainder 206 40))
                    (remainder (remainder 206 40)
                               (remainder 40
                                          (remainder 206 40))))))
;; 9: 7
(gcd (remainder (remainder 206 40)
                (remainder 40
                           (remainder 206 40)))
     (remainder (remainder 40
                           (remainder 206 40))
                (remainder (remainder 206 40)
                           (remainder 40
                                      (remainder 206 40)))))
;; 10: 14
(if (= (remainder (remainder 40
                             (remainder 206 40))
                  (remainder (remainder 206 40)
                             (remainder 40
                                        (remainder 206 40)))) 0) ; 7 remainder operations performed
    (remainder (remainder 206 40)
               (remainder 40
                          (remainder 206 40)))
    (gcd (remainder (remainder 40
                               (remainder 206 40))
                    (remainder (remainder 206 40)
                               (remainder 40
                                          (remainder 206 40))))
         (remainder (remainder (remainder 206 40)
                               (remainder 40
                                          (remainder 206 40)))
                    (remainder (remainder 40
                                          (remainder 206 40))
                               (remainder (remainder 206 40)
                                          (remainder 40
                                                     (remainder 206 40)))))))
(if (= (remainder (remainder 40
                             6)
                  (remainder (remainder 206 40)
                             (remainder 40
                                        (remainder 206 40)))) 0)
    (remainder (remainder 206 40)
               (remainder 40
                          (remainder 206 40)))
    (gcd (remainder (remainder 40
                               (remainder 206 40))
                    (remainder (remainder 206 40)
                               (remainder 40
                                          (remainder 206 40))))
         (remainder (remainder (remainder 206 40)
                               (remainder 40
                                          (remainder 206 40)))
                    (remainder (remainder 40
                                          (remainder 206 40))
                               (remainder (remainder 206 40)
                                          (remainder 40
                                                     (remainder 206 40)))))))
(if (= (remainder 4
                  (remainder (remainder 206 40)
                             (remainder 40
                                        (remainder 206 40)))) 0)
    (remainder (remainder 206 40)
               (remainder 40
                          (remainder 206 40)))
    (gcd (remainder (remainder 40
                               (remainder 206 40))
                    (remainder (remainder 206 40)
                               (remainder 40
                                          (remainder 206 40))))
         (remainder (remainder (remainder 206 40)
                               (remainder 40
                                          (remainder 206 40)))
                    (remainder (remainder 40
                                          (remainder 206 40))
                               (remainder (remainder 206 40)
                                          (remainder 40
                                                     (remainder 206 40)))))))
(if (= (remainder 4
                  (remainder 6
                             (remainder 40
                                        (remainder 206 40)))) 0)
    (remainder (remainder 206 40)
               (remainder 40
                          (remainder 206 40)))
    (gcd (remainder (remainder 40
                               (remainder 206 40))
                    (remainder (remainder 206 40)
                               (remainder 40
                                          (remainder 206 40))))
         (remainder (remainder (remainder 206 40)
                               (remainder 40
                                          (remainder 206 40)))
                    (remainder (remainder 40
                                          (remainder 206 40))
                               (remainder (remainder 206 40)
                                          (remainder 40
                                                     (remainder 206 40)))))))
(if (= (remainder 4
                  (remainder 6
                             (remainder 40
                                        6))) 0)
    (remainder (remainder 206 40)
               (remainder 40
                          (remainder 206 40)))
    (gcd (remainder (remainder 40
                               (remainder 206 40))
                    (remainder (remainder 206 40)
                               (remainder 40
                                          (remainder 206 40))))
         (remainder (remainder (remainder 206 40)
                               (remainder 40
                                          (remainder 206 40)))
                    (remainder (remainder 40
                                          (remainder 206 40))
                               (remainder (remainder 206 40)
                                          (remainder 40
                                                     (remainder 206 40)))))))
(if (= (remainder 4
                  (remainder 6
                             4)) 0)
    (remainder (remainder 206 40)
               (remainder 40
                          (remainder 206 40)))
    (gcd (remainder (remainder 40
                               (remainder 206 40))
                    (remainder (remainder 206 40)
                               (remainder 40
                                          (remainder 206 40))))
         (remainder (remainder (remainder 206 40)
                               (remainder 40
                                          (remainder 206 40)))
                    (remainder (remainder 40
                                          (remainder 206 40))
                               (remainder (remainder 206 40)
                                          (remainder 40
                                                     (remainder 206 40)))))))
(if (= (remainder 4
                  2) 0)
    (remainder (remainder 206 40)
               (remainder 40
                          (remainder 206 40)))
    (gcd (remainder (remainder 40
                               (remainder 206 40))
                    (remainder (remainder 206 40)
                               (remainder 40
                                          (remainder 206 40))))
         (remainder (remainder (remainder 206 40)
                               (remainder 40
                                          (remainder 206 40)))
                    (remainder (remainder 40
                                          (remainder 206 40))
                               (remainder (remainder 206 40)
                                          (remainder 40
                                                     (remainder 206 40)))))))
(if (= 0 0) ; true
    (remainder (remainder 206 40)
               (remainder 40
                          (remainder 206 40)))
    (gcd (remainder (remainder 40
                               (remainder 206 40))
                    (remainder (remainder 206 40)
                               (remainder 40
                                          (remainder 206 40))))
         (remainder (remainder (remainder 206 40)
                               (remainder 40
                                          (remainder 206 40)))
                    (remainder (remainder 40
                                          (remainder 206 40))
                               (remainder (remainder 206 40)
                                          (remainder 40
                                                     (remainder 206 40)))))))
;; 11: 18
(remainder (remainder 206 40)
           (remainder 40
                      (remainder 206 40))) ; 4 remainder operations performed
(remainder 6
           (remainder 40
                      (remainder 206 40)))
(remainder 6
           (remainder 40
                      6))
(remainder 6
           4)
2
; The GCD is 2
; A total of 18 remainder operations performed

;; Applicative order
;; 1: 0
(gcd 206 40)
;; 2: 0
(if (= 40 0) ; false
    206
    (gcd 40 (remainder 206 40)))
;; 3: 1
(gcd 40 (remainder 206 40)) ; 1 remainder operation performed
(gcd 40 6)
;; 4: 1
(if (= 6 0) ; false
    40
    (gcd 6 (remainder 40 6)))
;; 5: 2
(gcd 6 (remainder 40 6)) ; 1 remainder operation performed
(gcd 6 4)
;; 6: 2
(if (= 4 0)
    6
    (gcd 4 (remainder 6 4)))
;; 7: 3
(gcd 4 (remainder 6 4)) ; 1 remainder operation performed
(gcd 4 2)
;; 8: 3
(if (= 2 0)
    4
    (gcd 2 (remainder 4 2)))
;; 9: 4
(gcd 2 (remainder 4 2)) ; 1 remainder operation performed
(gcd 2 0)
;; 10: 4
(if (= 0 0)
    2
    (gcd 0 (remainder 2 0)))
;; 11: 4
2
; The GCD is 2
; A total of 4 remainder operations performed
