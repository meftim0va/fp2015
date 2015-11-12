#lang racket

(define (frac-arithmetic oper frac1 frac2)
  (cond
    [(= (cdr frac1) (cdr frac2)) (cons (oper (car frac1) (car frac2)) (cdr frac1))]
    [else
     (cons (oper 
            (* (car frac1) (cdr frac2)) 
            (* (cdr frac1) (car frac2)))
           (* (cdr frac1) (cdr frac2)))]))

(define (add-frac frac1 frac2)
  (frac-arithmetic + frac1 frac2))

(define (substract-frac frac1 frac2)
  (frac-arithmetic - frac1 frac2))

(define (mult-frac frac1 frac2)
    (cons (* (car frac1) (car frac2)) (* (cdr frac1) (cdr frac2))))

(define (simplify-frac frac)
  (define (iter nom denom count)
    (cond
      [(= nom denom) (cons 1 1)]
      [(< count 1) (cons nom denom)]
      [else 
       (if (and
            (= (remainder nom count) 0)
            (= (remainder denom count) 0))
           (iter (/ nom count) (/ denom count) (- count 1))
           (iter nom denom (- count 1)))]
       ))
  
  (iter (car frac) (cdr frac) (car frac)))