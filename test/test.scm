;; Core

(print 42)
(print -5)
(print 3.141592654)
(print -2.71828)
(print "Hello Scheme!")
(print #t)
(print #f)
(print foo)
(print foo?)
(print foo!)
(print foo=)
(print foo-bar)
(print foo/bar)
(print foo->bar)
(print foo->bar->baz)
(print foo.bar)
(print foo.bar.baz)
(newline)

(begin
  (define x 1)
  (define y (+ x 1))
  (print (+ x y)))

(define (times42 x) (* x 42))

(begin
  (define x 1)
  (set! x (times42 x))
  (print x))

(if (= x 42)
  (print #t)
  (print #f))

(if (> 1 2)
  (print "Huh?!"))

(define (rating x)
  (cond ((and (> x 0) (<= x 2)) "bad")
        ((= x 3) "okay")
        ((= x 4) "good")
        ((= x 5) "perfect")
        (else    "invalid")))

(print (rating -3))
(print (rating 1))
(print (rating 3))
(print (rating 5))
(newline)

(print
  (let ((a 1)
        (b 2)
        (c 3))
    (list a b c)))

(print
  (let ((a 1)
        (b 2)
        (c a)) ; no runtime error, evaluates to nil
    (list a b c)))

(print
  (let ((a 1)
        (b 2))
    (let ((c a))
      (list a b c))))

(print
  (letrec ((a 1)
           (b (+ a 1))
           (c (+ b 1)))
    (list a b c)))

(print
  (let ((a "foo")
        (b "bar"))
    (let ((a b)
          (b a))
      (list a b))))

(print
  (letrec ((a "foo")
           (b "bar"))
    (letrec ((a b)
             (b a))
      (list a b))))

(print
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

(newline)

;; quote/quasiquote

(print '1.23)
(print '#t)
(print 'foo)
(print 'foo.bar)
(print 'foo.bar.baz)
(print '(quote foo))
(print '(f o o))
(print '(1 2 '(3 4 5)))
(newline)

(print `1.23)
(print `#t)
(print `foo)
(print `(quasiquote foo))
(print `(f o o))
(print `(1 2 `(3 4 5)))
(newline)

(print (= 'a `a))
(print (= 'a `,a))
(print (equal? '(1 2 3) `(1 2 3)))
(print (equal? '(1 2 (+ 1 2)) `(1 2 (+ 1 2))))
(print (equal? (list 1 2 (+ 1 2)) `(1 2 ,(+ 1 2))))
(newline)

(print `(1 2 ,@(list 3 4) 5))
(print `(a 1 2 ,(+ 1 2) ,@(list 4 5) b))
(print (let ((x 3)) `(x has the value ,x)))
(print (letrec ((x '(3 4 5)) (y `(1 2 ,@x))) `(length of y is ,(length y))))
(print `(1 `,(+ 1 ,(+ 2 3)) 4))
(print `(1 ```,,@,,@(list (+ 1 2)) 4))
(newline)

;; Built-ins

(print (+ 1 2))
(print (- 4 3))
(print (* 5 4))
(print (/ 1 2))
(newline)

(print (= 1 1))
(print (= 2 3))
(print (> 6 7))
(print (> 8 7))
(print (< 7 8))
(print (< 9 8))
(print (>= 7 7))
(print (>= 7 8))
(print (<= 8 8))
(print (<= 9 8))
(newline)

(print (and #t #t))
(print (and #t #f))
(print (and #f #t))
(print (and #f #f))
(print (and #f (error)))
(print (or #t #t))
(print (or #t #f))
(print (or #f #t))
(print (or #f #f))
(print (or #t (error)))
(print (not #t))
(print (not #f))
(newline)

(print (list))
(print (list 1 2 (+ 1 2)))
(print (list 1 (list 2 (list 3))))
(newline)

(print (cons 1 '()))
(print (cons 'a '(b c)))
(print (cons (list 1) '(2 3)))
(newline)

(print (car '()))
(print (car '(1 2 3)))
(print (car '((a b c) e d f)))
(newline)

(print (cdr '()))
(print (cdr '(1)))
(print (cdr '(a b c (e d f))))
(newline)

(print (length '()))
(print (length '(1)))
(print (length '(1 (2))))
(print (length (list 'a 'b 'c)))
(newline)

(print (number? 1))
(print (number? 1.2))
(print (number? "3"))
(print (boolean? #t))
(print (boolean? #f))
(print (boolean? "#t"))
(print (string? "string?"))
(print (string? "two words"))
(print (string? 'string))
(print (symbol? "symbol?"))
(print (symbol? 'symbol))
(print (symbol? '+))
(print (lambda? 42))
(print (lambda? times42))
(print (lambda? +))
(newline)

(print (list? 1))
(print (list? "list"))
(print (list? '()))
(print (list? '(1 2 3)))
(newline)

(print (null? 'a))
(print (null? "list"))
(print (null? '()))
(print (null? '(1 2 3)))
(newline)

(print (pair? 3))
(print (pair? "pair"))
(print (pair? '()))
(print (pair? '(1 2 3)))
(newline)

(print (equal? 1 2))
(print (equal? (+ 1 2) 3))
(print (equal? #t #f))
(print (equal? 'a 'b))
(print (equal? "equal?" "equal?"))
(print (equal? '() '()))
(print (equal? '(1 + 2) '(1 + 2)))
(print (equal? '(1 2 3) '(1 2)))
(print (equal? '(a b c) '(a b c d)))
(print (equal? '(a b (c)) (list 'a 'b (list 'c))))
(newline)

(define x (read "(lambda (x) (* x 100))"))
(print ((eval x) 3))

(define y '(* (length (list 'quoted 'list)) 200))
(print (eval y))

(print (read "(lambda () #t)"))
(print (read "(lambda () '#f)"))
(print (read "(define (a) 'b)"))
(print (read "(define (a x y) '(b))"))
(newline)

(print (string? (show 1)))
(print (string? (show #t)))
(print (string? (show "show")))
(print (string? (show (+ 1 2))))
(print (show (list 'a 'b 'c)))
(print (show (read "(define (a x y) '(b))")))
(newline)

(print (string? (string-append "abc")))
(print (string-append "foo" "bar"))
(print (string-append (show 1) (show '+) (show 2) (show '=) (show (+ 1 2))))
(print (string-append 2 '+ 3 '= (+ 2 3))) ; show is called implicitly
(print (string-append "List of arguments of length " (length argv) ": " argv))
(newline)

;; "FFI"

(define lua "lua () return 1 end")
(print (lua))
(define lua "lua (a) return a + 1 end")
(print (lua 1))
(define lua "lua (a, b) return a + b end")
(print (lua 1 2))
(newline)

;; Prelude

(load "src/prelude.scm")

(define (fib n)
  (if (< n 2)
      n
      (+ (fib (- n 1)) (fib (- n 2)))))

(print (map fib (range 0 10 1)))
(newline)

;;

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
(newline)

;;

(define (countdown x)
  (lambda () (begin (set! x (- x 1)) x)))

(define c3 (countdown 3))

(print (begin (c3) (c3) (if (= (c3) 0) "Boom!")))
(newline)

;;

(define (plus x y)
  (let ((+ (lambda (x y)
             (if (and (list? x) (list? y))
                 (append x y)
                 (+ x y)))))
    (+ x y)))

(print (cons (plus 1 2) (plus '(4) '(5))))
(newline)

;;

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

(print (even? 42))
(print (odd? 42))
(print (even? 43))
(print (odd? 43))
(newline)

;;

(let= loop ((i 0)
            (l '()))
  (if (<= i 10)
      (loop (+ i 1) (cons (fib i) l))
      (print l)))

(let ((l '()))
  (begin
    (for i 0 10
         (set! l (cons (fib i) l)))
    (print l)))

(define (sum-list lst)
  (let= loop ((acc 0)
              (l lst))
    (cond ((null? l) acc)
          (else (loop (+ acc (car l)) (cdr l))))))

(print (sum-list (range 1 100 1)))
(newline)

;;

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

(print (directions '(North South South East West North West)))
