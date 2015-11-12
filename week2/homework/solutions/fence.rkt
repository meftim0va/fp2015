#lang racket

(define (string-repeat str n)
  (define (iter result count)
    (cond
      [(> count n) result]
      [else (iter (string-append result str) (add1 count))]))
  (iter "" 1))

(define (fence n)
  (string-append*
   (list
    "{"
    (string-repeat "-" (round (+ 1 (log n))))
    ">"
    (number->string n)
    "<"
    (string-repeat "-" (round (+ 1 (log n))))
    "}")))
