#lang racket

(define (series a b n)
  (define (iter result term count)
    (cond
      [(> count n) result]
      [else (iter term (+ term result) (+ count 1))]))
  (iter a b 2))

; -------------------------------------------------------- ;

(define (lucas n)
  (series 2 1 n))

(define (fibonacci n)
  (series 1 1 n))

(define (summed-member n)
  (+ (fibonacci n) (lucas n)))

; -------------------------------------------------------- ;

(define (nth-sum f n)
  (define (iter result count)
    (cond
      [(> count n) result]
      [else (+ result (iter (f count) (+ count 1)))]))
  (iter 0 1))

(define (nth-lucas-sum n)
  (nth-sum lucas n))

(define (nth-fibonacci-sum n)
  (nth-sum fibonacci n))

; -------------------------------------------------------- ;

(define (lucas-fib-diff n)
  (- (lucas n) (fibonacci n)))

