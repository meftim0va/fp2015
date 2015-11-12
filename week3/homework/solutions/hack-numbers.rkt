#lang racket

(require "../../../week2/homework/solutions/binary.rkt")

(provide
  palindrome?)

; -------------------------------------------------------- ;

(define (palindrome? str)
  (define (iter result count)
    (cond
      [(or (not result) (> count (/ (string-length str) 2))) result]
      [else (iter (string=? 
                    (~a (string-ref str count))
                    (~a (string-ref str (- (string-length str) count 1))))
                  (add1 count))]))
  (iter #t 0))

; -------------------------------------------------------- ;

(define (count-ones binary-str)
  (define (iter result count)
    (cond
      [(>= count (string-length binary-str)) result]
      [else (iter (+ result (or (and (string=? (~a (string-ref binary-str count)) "1") 1) 0)) (add1 count))]))
  (iter 0 0))

; -------------------------------------------------------- ;
    
(define (next-hack n)
  
  (define (is-hack? k)
    (define binary-str (to-binary-string k))
    (and (palindrome? binary-str) (odd? (count-ones binary-str))))
    
  (define m (add1 n))
  
  (cond
    [(is-hack? m) m]
    [else (next-hack m)]))
