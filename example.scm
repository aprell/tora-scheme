(load "tora/prelude.scm")

(define fib (lambda (n)
  (if (< n 2)
      n
      (+ (fib (- n 1)) (fib (- n 2))))))

(print (map fib '(0 1 2 3 4 5 6 7 8 9 10)))

;; SICP ----------------------------------------------------------------------

(define sqrt (lambda (x)
  (sqrt-iter 1.0 x)))

(define sqrt-iter (lambda (guess x)
  (if (good-enough? guess x)
      guess
      (sqrt-iter (improve guess x) x))))

(define good-enough? (lambda (guess x)
  (< (abs (- x (square guess))) 0.00001)))

(define abs (lambda (x)
  (if (< 0 x) x (- 0 x))))

(define square (lambda (x)
  (* x x)))

(define improve (lambda (guess x)
  (average guess (/ x guess))))

(define average (lambda (x y)
  (* 0.5 (+ x y))))

(print (square (sqrt 42)))

;; ---------------------------------------------------------------------------

(define countdown (lambda (x)
  (lambda () (begin (set! x (- x 1)) x))))

(define c3 (countdown 3))

(print
  (begin
    (c3)
    (c3)
    (if (= (c3) 0) "Boom!")))
