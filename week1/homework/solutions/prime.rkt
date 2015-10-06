#lang racket

(define (is-prime? n m)
  (if (= m 1)
    #t
    (if (> (remainder n m) 0) 
      (is-prime? n (- m 1))
      #f)))

(define (prime? n)
  (is-prime? n (quotient n 2)))
  
