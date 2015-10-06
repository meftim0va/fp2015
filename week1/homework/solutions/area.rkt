#lang racket

(define (area a b c)
  (* (/ 1 4) (sqrt (*
                     (+ a b c)
                     (+ (- a) b c)
                     (+ a (- b) c)
                     (+ a b (- c))))))

