#lang racket

(define (make-tree node left right)
  (list node left right))

(define (empty-tree? tree)
  (null? tree))

(define (root tree)
  (first tree))

(define (leaf? tree)
  (and (empty? (left tree)) (empty? (right tree))))

(define (left tree)
  (first (rest tree)))

(define (right tree)
  (first (rest (rest tree))))

(define (make-leaf node)
  (make-tree node '() '()))

(define (height tree)
  (cond
    [(empty? tree) 0]
    [else (+ 1 (max (height (left tree)) (height (right tree))))]))

(define (tree-level level tree)
  (cond
    [(empty? tree) (list)]
    [(= level 1) (list (root tree))]
    [else (append (tree-level (- level 1) (left tree)) (tree-level (- level 1) (right tree)))]))

(define (tree-levels tree)
  (map (lambda (level) (tree-level level tree))
       (range 1 (add1 (height tree)))))

(define (tree-map f tree)
  (cond
    [(empty? tree) tree]
    [else (make-tree (f (root tree)) (tree-map f (left tree)) (tree-map f (right tree)))]))

(define (bst-insert x tree)
  (cond
    [(empty? tree) (make-leaf x)]
    [(= x (root tree)) tree]
    [(< x (root tree)) (make-tree (root tree) (bst-insert x (left tree)) (right tree))]
    [(> x (root tree)) (make-tree (root tree) (left tree) (bst-insert x (right tree)))]))

(define (bst-element? x tree)
  (cond
    [(empty-tree? tree) #f]
    [(= x (root tree)) #t]
    [(< x (root tree)) (bst-element? x (left tree))]
    [else (bst-element? x (right tree))]))

(define (bst->list tree)
  (cond
    [(empty-tree? tree) '()]
    [else (append (bst->list (left tree)) (list (root tree)) (bst->list (right tree)))]))

(define (bst? tree)
  (cond [(leaf? tree) #t]
        [(or (> (root tree) (root (right tree))) (< (root tree) (root (left tree)))) #f]
        [else (and (bst? (left tree)) (bst? (right tree)))]))