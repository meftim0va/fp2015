#lang racket

(require "group.rkt")

(define (str->list str)
  (map (lambda (x) (make-string 1 x)) (string->list str)))

(define (run-length-encode str)
  (define (iter ls result)
    (cond [(empty? ls) result]
          [(= (length (first ls)) 1) (iter (rest ls) (string-append result (first (first ls))))]
          [else (iter (rest ls) (string-append result (number->string (length (first ls))) (first (first ls))))]))
  (iter (group (str->list str)) ""))
