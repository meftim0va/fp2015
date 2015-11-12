#lang racket

(require "cube-sums.rkt")

(define (count-cube-sums from to)
  (define (iter result count)
    (cond
      [(> count to) result]
      [else (iter (+ result (or (and (cube-sums? count) 1) 0)) (add1 count))]))
  (iter 0 from))