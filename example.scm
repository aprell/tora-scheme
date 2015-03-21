(load "tora/prelude.scm")

(define (fib n)
  (if (< n 2)
      n
      (+ (fib (- n 1)) (fib (- n 2)))))

(print (map fib '(0 1 2 3 4 5 6 7 8 9 10)))

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

(print (cons (plus 1 2) (plus '(4) '(5))))

(print
    (letrec ((is-even?
               (lambda (n)
                 (if (= n 0) #t
                     (is-odd? (- n 1)))))
             (is-odd?
               (lambda (n)
                 (if (= n 0) #f
                     (is-even? (- n 1))))))
      (is-even? 42)))
