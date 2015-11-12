#lang racket

(provide
  to-binary-string)

(define (string-reverse str)
  (define (iter result count)
    (cond
      [(< count 0) result]
      [else (iter (string-append result (~a (string-ref str count))) (- count 1))]))
  (iter "" (- (string-length str) 1)))

(define (to-binary-string n)
  (define (iter result number)
    (cond
      [(= number 0) (string-reverse result)]
      [else (iter (string-append result (~a (remainder number 2))) (quotient number 2))]))
  (iter "" n))

(define (from-binary-string binary-str)
  (define (iter result number count)
    (cond
      [(= number 0) result]
      [else (iter (+ result (* (expt 2 count) (remainder number 10))) (quotient number 10) (add1 count))]))

  (iter 0 (string->number binary-str) 0))