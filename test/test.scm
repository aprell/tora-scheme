;; Core

(print 42)
(print -5)
(print 3.141592654)
(print -2.71828)
(print "Hello Scheme!")
(print #t)
(print #f)
(print foo)
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
