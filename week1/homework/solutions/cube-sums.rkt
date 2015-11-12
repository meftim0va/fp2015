#lang racket

(provide
  cube-sums?)

(define (cube? n)
  (define (iter result count)
    (cond
      [(or result (> count (exact-ceiling (expt n 1/3)))) result]
      [else (iter (= (expt count 3) n) (add1 count))]))
  (iter #f 1))
      

(define (cube-sums? n)
  (define (iter result count)
    (cond
      [(or result (> (expt count 3) n)) result]
      [else (iter (cube? (- n (expt count 3))) (add1 count))]))
  (iter #f 1))

