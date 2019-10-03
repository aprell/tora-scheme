(load "tora/prelude.scm")

(define (fib n)
  (if (< n 2)
      n
      (+ (fib (- n 1)) (fib (- n 2)))))

(assert-equal
  (map fib (range 0 10 1))
  '(0 1 1 2 3 5 8 13 21 34 55))

;; SICP ----------------------------------------------------------------------

(define (sqrt x)
  (sqrt-iter 1.0 x))

(define (sqrt-iter guess x)
  (if (good-enough? guess x)
      guess
      (sqrt-iter (improve guess x) x)))

(define (good-enough? guess x)
  (< (abs (- x (square guess))) 0.00001))

(define (improve guess x)
  (average guess (/ x guess)))

(define (average x y)
  (* 0.5 (+ x y)))

(print (square (sqrt 42)))

;; ---------------------------------------------------------------------------

(define (countdown x)
  (lambda () (begin (set! x (- x 1)) x)))

(define c3 (countdown 3))

(print
  (begin
    (c3)
    (c3)
    (if (= (c3) 0) "Boom!")))

;; ---------------------------------------------------------------------------

(define (plus x y)
  (let ((+ (lambda (x y)
             (if (and (list? x) (list? y))
                 (append x y)
                 (+ x y)))))
    (+ x y)))

(assert-equal
  (cons (plus 1 2) (plus '(4) '(5)))
  '(3 4 5))

(define (even? n)
  (letrec ((is-even?
             (lambda (n)
               (if (= n 0) #t
                   (is-odd? (- n 1)))))
           (is-odd?
             (lambda (n)
               (if (= n 0) #f
                   (is-even? (- n 1))))))
    (is-even? n)))

(define (odd? n)
  (not (even? n)))

(assert-true (even? 42))
(assert-false (odd? 42))
(assert-false (even? 43))
(assert-true (odd? 43))

;; ---------------------------------------------------------------------------

(let= loop ((i 0)
            (l '()))
  (if (<= i 10)
      (loop (+ i 1) (cons (fib i) l))
      (assert-equal l '(55 34 21 13 8 5 3 2 1 1 0))))

(let ((l '()))
  (begin
    (for i 0 10
         (set! l (cons (fib i) l)))
    (assert-equal l '(55 34 21 13 8 5 3 2 1 1 0))))

(define (sum-list lst)
  (let= loop ((acc 0)
              (l lst))
    (cond ((null? l) acc)
          (else (loop (+ acc (car l)) (cdr l))))))

(assert-equal
  (sum-list (range 1 100 1))
  5050)

;; ---------------------------------------------------------------------------

(define (simplify dir lst)
  (cond
    ((and (equal? dir 'North) (equal? (car lst) 'South)) (cdr lst))
    ((and (equal? dir 'South) (equal? (car lst) 'North)) (cdr lst))
    ((and (equal? dir 'East)  (equal? (car lst) 'West))  (cdr lst))
    ((and (equal? dir 'West)  (equal? (car lst) 'East))  (cdr lst))
    (else (cons dir lst))))

(define (directions lst)
  (let ((simplified-list (foldl (flip simplify) '() lst)))
    (reverse simplified-list)))

(assert-equal
  (directions '(North South South East West North West))
  '(West))
