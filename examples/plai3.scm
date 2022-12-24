;; +--------------------------------------------------------------------------+
;; | Code transcribed from                                                    |
;; |   Programming Languages: Application and Interpretation, 3rd Edition     |
;; +--------------------------------------------------------------------------+
;; | https://www.plai.org                                                     |
;; +--------------------------------------------------------------------------+

(load "test/utils.scm")

;; ----- Evaluating Local Binding -----

(test
  (let ((x 1))
    (+ x x))
  2)

(test
  (let ((x 1))
    (let ((y 2))
      (+ x y)))
  3)

(test
  (let ((x 1))
    (let ((y 2))
      (let ((x 3))
        (+ x y))))
  5)

(test
  (let ((x 1))
    (+ (let ((x 2)) x)
       x))
  3)

;; ----- Evaluating Functions -----

(test
  (let ((x 1))
    (let ((f (lambda (y) x)))
      (let ((x 2))
        (f 10))))
  1)

(test
  ((let ((x 3))
     (lambda (y) (+ x y)))
   4)
  7)

(test
  ((let ((y 3))
    (lambda (y) (+ y 1)))
   5)
  6)

;; ----- More on Macros -----

(test/true
  (let ((x #t))
    (or #f x)))

(test/exn
  (let ((not 1))
    (unless #f
      (+ not 1)))
  "eval: attempt to call '1' (a non-function value)")

;; ----- A Standard Model of Objects -----

(define (o m)
  (switch m
    (('add1 (lambda (x) (+ x 1)))
     ('sub1 (lambda (x) (- x 1))))))

(test ((o 'add1) 5) 6)
(test ((o (car '(add1))) 5) 6)

(define (mk-counter init)
  (let ((count init))
    (lambda (m)
      (switch m
        (('inc (lambda () (set! count (+ count 1))))
         ('dec (lambda () (set! count (- count 1))))
         ('get (lambda () count)))))))

(test
  (let ((counter (mk-counter 5)))
    (begin
      ((counter 'inc))
      ((counter 'inc))
      ((counter 'dec))
      ((counter 'get))))
  6)

(define o-self
  (let ((self 'null))
    (begin
      (set! self
        (lambda (m)
          (switch m
            (('add1 (lambda (x) ((self 'sub1) (+ x 2))))
             ('sub1 (lambda (x) (- x 1)))))))
      self)))

(test ((o-self 'add1) 5) 6)
(test ((o-self (car '(add1))) 5) 6)

(define o-self/2
  (lambda (m)
    (switch m
      (('add1 (lambda (self x) ((self 'sub1) self (+ x 2))))
       ('sub1 (lambda (self x) (- x 1)))))))

(test ((o-self/2 'add1) o-self/2 5) 6)
(test ((o-self/2 (car '(add1))) o-self/2 5) 6)

(define (leaf)
  (let ((self 'null))
    (begin
      (set! self
        (lambda (m)
          (switch m
            (('sum (lambda () 0))))))
      self)))

(define (node l v r)
  (let ((self 'null))
    (begin
      (set! self
        (lambda (m)
          (switch m
            (('sum (lambda () (+ v
                                 (+ ((l 'sum))
                                    ((r 'sum))))))))))
      self)))

(define a-tree
  (node (node (leaf) 5 (leaf))
        10
        (node (node (leaf) 6 (leaf)) 15 (leaf))))

(test ((a-tree 'sum)) 36)

;; ----- What Else Do Objects Have? -----

(define (leaf-with-size base)
  (let ((parent (base))
        (self 'null))
    (begin
      (set! self
        (lambda (m)
          (switch m
            (('size (lambda () 0))
             (default (parent m))))))
      self)))

(define (node-with-size base l v r)
  (let ((parent (base l v r))
        (self 'null))
    (begin
      (set! self
        (lambda (m)
          (switch m
            (('size (lambda () (+ 1
                                  (+ ((l 'size))
                                     ((r 'size))))))
             (default (parent m))))))
      self)))

(define a-tree-with-size
  (node-with-size node (node-with-size node (leaf-with-size leaf) 5 (leaf-with-size leaf))
                  10
                  (node-with-size node (node-with-size node (leaf-with-size leaf) 6 (leaf-with-size leaf)) 15 (leaf-with-size leaf))))

(test ((a-tree-with-size 'sum)) 36)
(test ((a-tree-with-size 'size)) 4)

(test/result)
