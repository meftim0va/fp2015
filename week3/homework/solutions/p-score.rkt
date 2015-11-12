#lang racket

(require "hack-numbers.rkt")

(define (reverse-number n)
  (define (iter result k)
    (cond
      [(= k 0) result]
      [else (iter (+ (* result 10) (remainder k 10)) (quotient k 10))]))
  (iter 0 n))

(define (p-score n)
  (define (iter result count)
    (cond
      [(palindrome? (number->string result)) count]
      [else (iter (+ result (reverse-number result)) (add1 count))]))
  (iter n 1))