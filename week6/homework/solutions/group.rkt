#lang racket

(provide group)

(define (group items)
  (define (take-while pred items)
    (cond
      [(or (empty? items) (not (pred (first items)))) (list)]
      [else (cons (first items) (take-while pred (rest items)))]))
  (define (drop-while pred items)
    (cond
      [(or (empty? items) (not (pred (first items)))) items]
      [else (drop-while pred (rest items))]))
 
  (cond 
      [(null? items) (list)]
      [else (cons
          (take-while (lambda (x) (equal? x (first items))) items)
          (group (drop-while (lambda (x) (equal? x (first items))) items)))]))

