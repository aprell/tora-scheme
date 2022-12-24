(load "test/utils.scm")

;; ----- Core -----

(test 42 42)
(test -5 -5)
(test 3.141592654 3.141592654)
(test -2.71828 -2.71828)
(test "Hello Scheme!" "Hello Scheme!")
(test/true #t)
(test/false #f)
(test/nil foo)
(test/nil foo?)
(test/nil foo!)
(test/nil foo=)
(test/nil foo-bar)
(test/nil foo/bar)
(test/nil foo->bar)
(test/nil foo->bar->baz)
(test/nil foo.bar)
(test/nil foo.bar.baz)

(test
  (begin
    (define x 1)
    (define y (+ x 1))
    (+ x y))
  3)

(define (times42 x) (* x 42))

(test
  (begin
    (define x 1)
    (set! x ((lambda (x) (* x 42)) x))
    x)
  42)

(test/true (if (= x 42) #t #f))
(test/nil  (if (> 1 2) (print "Huh?!")))

(define (rating x)
  (cond ((and (> x 0) (<= x 2)) "bad")
        ((= x 3) "okay")
        ((= x 4) "good")
        ((= x 5) "perfect")
        (else    "invalid")))

(test (rating -3) "invalid")
(test (rating 1)  "bad")
(test (rating 3)  "okay")
(test (rating 5)  "perfect")

(test
  (let ((a 1)
        (b 2)
        (c 3))
    (list a b c))
  '(1 2 3))

(test
  (let ((a 1)
        (b 2)
        (c a)) ; no runtime error, evaluates to nil
    (list a b c))
  '(1 2))

(test
  (let ((a 1)
        (b 2))
    (let ((c a))
      (list a b c)))
  '(1 2 1))

(test
  (letrec ((a 1)
           (b (+ a 1))
           (c (+ b 1)))
    (list a b c))
  '(1 2 3))

(test
  (let ((a "foo")
        (b "bar"))
    (let ((a b)
          (b a))
      (list a b)))
  '("bar" "foo"))

(test
  (letrec ((a "foo")
           (b "bar"))
    (letrec ((a b)
             (b a))
      (list a b)))
  '("bar" "bar"))

(test/true
  (let ((xs
          (let ((xs (list "a")))
            (let ((xs (cons "b" xs)))
              (let ((xs (cons "c" xs)))
                xs))))
        (ys
          (letrec ((ys (list "a"))
                   (ys (cons "b" ys))
                   (ys (cons "c" ys)))
            ys)))
    (equal? xs ys)))

;; ----- quote/quasiquote -----

(test '1.23 (quote 1.23))
(test '#t (quote #t))
(test 'foo (quote foo))
(test 'foo.bar (quote foo.bar))
(test 'foo.bar.baz (quote foo.bar.baz))
(test '(quote foo) (quote (quote foo)))
(test '(f o o) (quote (f o o)))
(test '(1 2 '(3 4 5)) (quote (1 2 (quote (3 4 5)))))

(test `1.23 (quote 1.23))
(test `#t (quote #t))
(test `foo (quote foo))
(test `(quasiquote foo) (quote (quasiquote foo)))
(test `(f o o) (quote (f o o)))
(test `(1 2 `(3 4 5)) (quote (1 2 (quasiquote (3 4 5)))))

(test/true  (= 'a `a))
(test/false (= 'a `,a))
(test/true  (equal? '(1 2 3) `(1 2 3)))
(test/true  (equal? '(1 2 (+ 1 2)) `(1 2 (+ 1 2))))
(test/true  (equal? (list 1 2 (+ 1 2)) `(1 2 ,(+ 1 2))))

(test `(1 2 ,@(list 3 4) 5) (quote (1 2 3 4 5)))
(test `(a 1 2 ,(+ 1 2) ,@(list 4 5) b) (quote (a 1 2 3 4 5 b)))
(test (let ((x 3)) `(x has the value ,x)) (quote (x has the value 3)))

(test (letrec ((x '(3 4 5)) (y `(1 2 ,@x))) `(length of y is ,(length y)))
      (quote (length of y is 5)))
(test `(1 `,(+ 1 ,(+ 2 3)) 4)
      (quote (1 (quasiquote (unquote (+ 1 5))) 4)))
(test `(1 ```,,@,,@(list (+ 1 2)) 4)
      (quote (1 (quasiquote (quasiquote (quasiquote (unquote (unquote-splicing (unquote 3)))))) 4)))

;; ----- Built-ins -----

(test (+ 1 2) 3)
(test (- 4 3) 1)
(test (* 5 4) 20)
(test (/ 1 2) 0.5)

(test/true  (= 1 1))
(test/false (= 2 3))
(test/false (> 6 7))
(test/true  (> 8 7))
(test/true  (< 7 8))
(test/false (< 9 8))
(test/true  (>= 7 7))
(test/false (>= 7 8))
(test/true  (<= 8 8))
(test/false (<= 9 8))

(test/true  (and #t #t))
(test/false (and #t #f))
(test/false (and #f #t))
(test/false (and #f #f))
(test/false (and #f (error)))
(test/true  (or #t #t))
(test/true  (or #t #f))
(test/true  (or #f #t))
(test/false (or #f #f))
(test/true  (or #t (error)))
(test/false (not #t))
(test/true  (not #f))

(test (list) '())
(test (list 1 2 (+ 1 2)) '(1 2 3))
(test (list 1 (list 2 (list 3))) '(1 (2 (3))))

(test (cons 1 '()) '(1))
(test (cons 'a '(b c)) '(a b c))
(test (cons (list 1) '(2 3)) '((1) 2 3))

(test/nil (car '()))
(test (car '(1 2 3)) 1)
(test (car '((a b c) e d f)) '(a b c))

(test (cdr '()) '())
(test (cdr '(1)) '())
(test (cdr '(a b c (e d f))) '(b c (e d f)))

(test (length '()) 0)
(test (length '(1)) 1)
(test (length '(1 (2))) 2)
(test (length (list 'a 'b 'c)) 3)

(test/true  (number? 1))
(test/true  (number? 1.2))
(test/false (number? "3"))
(test/true  (boolean? #t))
(test/true  (boolean? #f))
(test/false (boolean? "#t"))
(test/true  (string? "string?"))
(test/true  (string? "two words"))
(test/false (string? 'string))
(test/false (symbol? "symbol?"))
(test/true  (symbol? 'symbol))
(test/true  (symbol? '+))
(test/false (lambda? 42))
(test/true  (lambda? times42))
(test/true  (lambda? +))

(test/false (list? 1))
(test/false (list? "list"))
(test/true  (list? '()))
(test/true  (list? '(1 2 3)))

(test/false (null? 'a))
(test/false (null? "list"))
(test/true  (null? '()))
(test/false (null? '(1 2 3)))

(test/false (pair? 3))
(test/false (pair? "pair"))
(test/false (pair? '()))
(test/true  (pair? '(1 2 3)))

(test/false (equal? 1 2))
(test/true  (equal? (+ 1 2) 3))
(test/false (equal? #t #f))
(test/false (equal? 'a 'b))
(test/true  (equal? "equal?" "equal?"))
(test/true  (equal? '() '()))
(test/true  (equal? '(1 + 2) '(1 + 2)))
(test/false (equal? '(1 2 3) '(1 2)))

(test/false (equal? '(a b c) '(a b c d)))
(test/true  (equal? '(a b (c)) (list 'a 'b (list 'c))))

(test (read "(lambda () #t)") '(lambda () #t))
(test (read "(lambda () '#f)") '(lambda () (quote #f)))
(test (read "(define (a) 'b)") '(define a (lambda () (quote b))))
(test (read "(define (a x y) '(b))") '(define a (lambda (x y) (quote (b)))))

(test ((eval (read "(lambda (x) (* x 100))")) 3) 300)
(test (eval '(* (length (list 'quoted 'list)) 200)) 400)
(test (eval/exn '(let ((x 1)) (cond (else #f) ((= x 1) #t))))
      "eval: else must be last cond-clause")

(test/true (string? (show 1)))
(test/true (string? (show #t)))
(test/true (string? (show "show")))
(test/true (string? (show (+ 1 2))))
(test (show (list 'a 'b 'c)) "(a b c)")
(test (show (read "(define (a x y) '(b))")) "(define a (lambda (x y) (quote (b))))")

(test/true (string? (string-append "abc")))
(test (string-append "foo" "bar") "foobar")
(test (string-append (show 1) (show '+) (show 2) (show '=) (show (+ 1 2))) "1+2=3")
(test (string-append 2 '+ 3 '= (+ 2 3)) "2+3=5") ; show is called implicitly
(test (string-append "List of arguments of length " (length argv) ": " argv)
      "List of arguments of length 1: (test/test.scm)")

;; ----- Macros -----

(define-macro (infix a op b) `(,op ,a ,b))
(test (infix 2 - 1) 1)

(define-macro (commute a op b) `(infix ,b ,op ,a))
(test (commute 2 - 1) -1)

;; ------ "FFI" ------

(define lua "lua () return 1 end")
(test (lua) 1)

(define lua "lua (a) return a + 1 end")
(test (lua 1) 2)

(define lua "lua (a, b) return a + b end")
(test (lua 1 2) 3)

(define round "lua (n) return tonumber(('%.5f'):format(n)) end")
(test (round 0.1) 0.1)
(test (round 0.12) 0.12)
(test (round 0.123) 0.123)
(test (round 0.1234) 0.1234)
(test (round 0.12345) 0.12345)
(test (round 0.123456) 0.12346)

;; ----- Prelude -----

(load "src/prelude.scm")

(define (fib n)
  (if (< n 2)
      n
      (+ (fib (- n 1)) (fib (- n 2)))))

(test (map fib (range 0 10 1))
      '(0 1 1 2 3 5 8 13 21 34 55))

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

(test (round (square (sqrt 42))) 42)

(define (countdown x)
  (lambda () (begin (set! x (- x 1)) x)))

(define c3 (countdown 3))

(test (begin (c3) (c3) (if (= (c3) 0) "Boom!")) "Boom!")

(define (plus x y)
  (let ((+ (lambda (x y)
             (if (and (list? x) (list? y))
                 (append x y)
                 (+ x y)))))
    (+ x y)))

(test (cons (plus 1 2) (plus '(4) '(5))) '(3 4 5))

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

(test/true  (even? 42))
(test/false (odd? 42))
(test/false (even? 43))
(test/true  (odd? 43))

(test
  (let= loop ((i 0)
              (l '()))
        (if (<= i 10)
            (loop (+ i 1) (cons (fib i) l))
            l))
  '(55 34 21 13 8 5 3 2 1 1 0))

(test
  (let ((l '()))
    (begin
      (for i 0 10
           (set! l (cons (fib i) l)))
      l))
  '(55 34 21 13 8 5 3 2 1 1 0))

(define (sum-list lst)
  (let= loop ((acc 0)
              (l lst))
    (cond ((null? l) acc)
          (else (loop (+ acc (car l)) (cdr l))))))

(test (sum-list (range 1 100 1)) 5050)

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

(test (directions '(North South South East West North West)) '(West))

(test/result)
