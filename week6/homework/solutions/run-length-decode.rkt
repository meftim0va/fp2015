#lang racket

(require "group.rkt")

(define (str->list str)
  (map (lambda (x) (make-string 1 x)) (string->list str)))

(define (run-length-decode str)
  (define (append-while count symbol result)
    (if (zero? count) result (append-while (- count 1) symbol (string-append symbol result))))
  (define (iter count ls result)
    (cond [(empty? ls) result]
          [(string->number (first ls)) (iter (+ (* (- count 1) 10) (string->number (first ls))) (rest ls) result)]
          [else (iter 1 (rest ls) (string-append result (append-while count (first ls) "")) )]))
  (iter 1 (str->list str) ""))