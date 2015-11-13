#lang racket

(define (sum numbers)
  (define (iter result list)
    (cond
      [(empty? list) result]
      [else (iter (+ result (car list)) (cdr list))]))
  
  (iter 0 numbers))

; -------------------------------------------------------- ;

(define (member? x items)
  (define (iter result list)
    (cond
      [(or result (empty? list)) result]
      [else (iter (eq? x (car list)) (cdr list))]))
  
  (iter #f items))

; -------------------------------------------------------- ;

(define (length2 items)
  (define (iter result list)
    (cond
      [(empty? list) result]
      [else (iter (add1 result) (cdr list))]))

  (iter 0 items))

; -------------------------------------------------------- ;

(define (list-ref2 items n)
  (define (iter result count list)
    (cond
      [(> count n) result]
      [else (iter (car list) (add1 count) (cdr list))]))
  
  (iter (list) 0 items))

; -------------------------------------------------------- ;

(define (range2 a b)
  (define (iter result count)
    (cond
      [(< count a) result]
      [else (iter (cons count result) (- count 1))]))

  (iter (list) (- b 1)))

; -------------------------------------------------------- ;

(define (build-list2 n f)
  (map f (range2 0 n)))

; -------------------------------------------------------- ;

(define (append2 l1 l2)
  (define (iter list1 list2)
    (cond
      [(empty? list1) list2]
      [else (cons (car list1) (iter (cdr list1) list2))]))

  (iter l1 l2))

; -------------------------------------------------------- ;

(define (reverse2 items)
  (define (iter result list)
    (cond
      [(empty? list) result]
      [else (iter (cons (car list) result) (cdr list))]))
  
  (iter '() items))

; -------------------------------------------------------- ;

(define (take2 n items)
   (define (iter count list)
    (cond [(or (empty? list) (= n count)) (list)]
          [else (cons (car list) (iter (add1 count) (cdr list)))]))
  
  (iter 0 items))

; -------------------------------------------------------- ;

(define (drop2 n items)
  (define (iter count result)
    (cond
      [(> count n) result]
      [else (iter (add1 count) (cdr result))]))

  (iter 1 items))

; -------------------------------------------------------- ;

(define (take-while p items)
  (define (iter count list)
    (cond
      [(not (p (car list))) (take2 count items)]
      [else (iter (add1 count) (cdr list))]))

  (iter 0 items))

; -------------------------------------------------------- ;

(define (drop-while p items)
  (cond
    [(not (p (car items))) items]
    [else (drop-while p (cdr items))]))

; -------------------------------------------------------- ;

(define (number->list n)
  (define (iter result k)
    (cond
      [(zero? k) result]
      [else (iter (cons (remainder k 10) result) (quotient k 10))]))

  (iter (list) n))

; -------------------------------------------------------- ;

(define (list->number ns)
  (define (iter result list)
    (cond
      [(empty? list) result]
      [else (iter (+ (* result 10) (car list)) (cdr list))]))
  
  (iter 0 ns))

