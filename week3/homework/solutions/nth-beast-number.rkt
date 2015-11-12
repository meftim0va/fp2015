#lang racket

(define (nth-beast-number n)
  (define (iter result count)
    (cond
      [(> count n) result]
      [else (iter (+ (* result 1000) 666) (add1 count))]))
  (iter 0 1))
