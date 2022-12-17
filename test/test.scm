;; ----- Core -----

(print 42)               ;; CHECK: 42
(print -5)               ;; CHECK: -5
(print 3.141592654)      ;; CHECK: 3.141592654
(print -2.71828)         ;; CHECK: -2.71828
(print "Hello Scheme!")  ;; CHECK: Hello Scheme!
(print #t)               ;; CHECK: #t
(print #f)               ;; CHECK: #f
(print foo)              ;; CHECK: nil
(print foo?)             ;; CHECK: nil
(print foo!)             ;; CHECK: nil
(print foo=)             ;; CHECK: nil
(print foo-bar)          ;; CHECK: nil
(print foo/bar)          ;; CHECK: nil
(print foo->bar)         ;; CHECK: nil
(print foo->bar->baz)    ;; CHECK: nil
(print foo.bar)          ;; CHECK: nil
(print foo.bar.baz)      ;; CHECK: nil

(begin
  (define x 1)
  (define y (+ x 1))
  (print (+ x y)))

;; CHECK: 3

(define (times42 x) (* x 42))

(begin
  (define x 1)
  (set! x (times42 x))
  (print x))

;; CHECK: 42

(if (= x 42)
  (print #t)
  (print #f))

;; CHECK: #t

(if (> 1 2)
  (print "Huh?!"))

(define (rating x)
  (cond ((and (> x 0) (<= x 2)) "bad")
        ((= x 3) "okay")
        ((= x 4) "good")
        ((= x 5) "perfect")
        (else    "invalid")))

(print (rating -3))  ;; CHECK: invalid
(print (rating 1))   ;; CHECK: bad
(print (rating 3))   ;; CHECK: okay
(print (rating 5))   ;; CHECK: perfect

(print
  (let ((a 1)
        (b 2)
        (c 3))
    (list a b c)))

;; CHECK: (1 2 3)

(print
  (let ((a 1)
        (b 2)
        (c a)) ; no runtime error, evaluates to nil
    (list a b c)))

;; CHECK: (1 2)

(print
  (let ((a 1)
        (b 2))
    (let ((c a))
      (list a b c))))

;; CHECK: (1 2 1)

(print
  (letrec ((a 1)
           (b (+ a 1))
           (c (+ b 1)))
    (list a b c)))

;; CHECK: (1 2 3)

(print
  (let ((a "foo")
        (b "bar"))
    (let ((a b)
          (b a))
      (list a b))))

;; CHECK: (bar foo)

(print
  (letrec ((a "foo")
           (b "bar"))
    (letrec ((a b)
             (b a))
      (list a b))))

;; CHECK: (bar bar)

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

;; CHECK: #t

;; ----- quote/quasiquote -----

(print '1.23)              ;; CHECK: 1.23
(print '#t)                ;; CHECK: #t
(print 'foo)               ;; CHECK: foo
(print 'foo.bar)           ;; CHECK: foo.bar
(print 'foo.bar.baz)       ;; CHECK: foo.bar.baz
(print '(quote foo))       ;; CHECK: (quote foo)
(print '(f o o))           ;; CHECK: (f o o)
(print '(1 2 '(3 4 5)))    ;; CHECK: (1 2 (quote (3 4 5)))

(print `1.23)              ;; CHECK: 1.23
(print `#t)                ;; CHECK: #t
(print `foo)               ;; CHECK: foo
(print `(quasiquote foo))  ;; CHECK: (quasiquote foo)
(print `(f o o))           ;; CHECK: (f o o)
(print `(1 2 `(3 4 5)))    ;; CHECK: (1 2 (quasiquote (3 4 5)))

(print (= 'a `a))                                    ;; CHECK: #t
(print (= 'a `,a))                                   ;; CHECK: #f
(print (equal? '(1 2 3) `(1 2 3)))                   ;; CHECK: #t
(print (equal? '(1 2 (+ 1 2)) `(1 2 (+ 1 2))))       ;; CHECK: #t
(print (equal? (list 1 2 (+ 1 2)) `(1 2 ,(+ 1 2))))  ;; CHECK: #t

(print `(1 2 ,@(list 3 4) 5))
(print `(a 1 2 ,(+ 1 2) ,@(list 4 5) b))
(print (let ((x 3)) `(x has the value ,x)))
(print (letrec ((x '(3 4 5)) (y `(1 2 ,@x))) `(length of y is ,(length y))))
(print `(1 `,(+ 1 ,(+ 2 3)) 4))
(print `(1 ```,,@,,@(list (+ 1 2)) 4))

;; CHECK: (1 2 3 4 5)
;; CHECK: (a 1 2 3 4 5 b)
;; CHECK: (x has the value 3)
;; CHECK: (length of y is 5)
;; CHECK: (1 (quasiquote (unquote (+ 1 5))) 4)
;; CHECK: (1 (quasiquote (quasiquote (quasiquote (unquote (unquote-splicing (unquote 3)))))) 4)

;; ----- Built-ins -----

(print (+ 1 2))                     ;; CHECK: 3
(print (- 4 3))                     ;; CHECK: 1
(print (* 5 4))                     ;; CHECK: 20
(print (/ 1 2))                     ;; CHECK: 0.5

(print (= 1 1))                     ;; CHECK: #t
(print (= 2 3))                     ;; CHECK: #f
(print (> 6 7))                     ;; CHECK: #f
(print (> 8 7))                     ;; CHECK: #t
(print (< 7 8))                     ;; CHECK: #t
(print (< 9 8))                     ;; CHECK: #f
(print (>= 7 7))                    ;; CHECK: #t
(print (>= 7 8))                    ;; CHECK: #f
(print (<= 8 8))                    ;; CHECK: #t
(print (<= 9 8))                    ;; CHECK: #f

(print (and #t #t))                 ;; CHECK: #t
(print (and #t #f))                 ;; CHECK: #f
(print (and #f #t))                 ;; CHECK: #f
(print (and #f #f))                 ;; CHECK: #f
(print (and #f (error)))            ;; CHECK: #f
(print (or #t #t))                  ;; CHECK: #t
(print (or #t #f))                  ;; CHECK: #t
(print (or #f #t))                  ;; CHECK: #t
(print (or #f #f))                  ;; CHECK: #f
(print (or #t (error)))             ;; CHECK: #t
(print (not #t))                    ;; CHECK: #f
(print (not #f))                    ;; CHECK: #t

(print (list))                      ;; CHECK: ()
(print (list 1 2 (+ 1 2)))          ;; CHECK: (1 2 3)
(print (list 1 (list 2 (list 3))))  ;; CHECK: (1 (2 (3)))

(print (cons 1 '()))                ;; CHECK: (1)
(print (cons 'a '(b c)))            ;; CHECK: (a b c)
(print (cons (list 1) '(2 3)))      ;; CHECK: ((1) 2 3)

(print (car '()))                   ;; CHECK: nil
(print (car '(1 2 3)))              ;; CHECK: 1
(print (car '((a b c) e d f)))      ;; CHECK: (a b c)

(print (cdr '()))                   ;; CHECK: ()
(print (cdr '(1)))                  ;; CHECK: ()
(print (cdr '(a b c (e d f))))      ;; CHECK: (b c (e d f))

(print (length '()))                ;; CHECK: 0
(print (length '(1)))               ;; CHECK: 1
(print (length '(1 (2))))           ;; CHECK: 2
(print (length (list 'a 'b 'c)))    ;; CHECK: 3

(print (number? 1))                 ;; CHECK: #t
(print (number? 1.2))               ;; CHECK: #t
(print (number? "3"))               ;; CHECK: #f
(print (boolean? #t))               ;; CHECK: #t
(print (boolean? #f))               ;; CHECK: #t
(print (boolean? "#t"))             ;; CHECK: #f
(print (string? "string?"))         ;; CHECK: #t
(print (string? "two words"))       ;; CHECK: #t
(print (string? 'string))           ;; CHECK: #f
(print (symbol? "symbol?"))         ;; CHECK: #f
(print (symbol? 'symbol))           ;; CHECK: #t
(print (symbol? '+))                ;; CHECK: #t
(print (lambda? 42))                ;; CHECK: #f
(print (lambda? times42))           ;; CHECK: #t
(print (lambda? +))                 ;; CHECK: #t

(print (list? 1))                   ;; CHECK: #f
(print (list? "list"))              ;; CHECK: #f
(print (list? '()))                 ;; CHECK: #t
(print (list? '(1 2 3)))            ;; CHECK: #t

(print (null? 'a))                  ;; CHECK: #f
(print (null? "list"))              ;; CHECK: #f
(print (null? '()))                 ;; CHECK: #t
(print (null? '(1 2 3)))            ;; CHECK: #f

(print (pair? 3))                   ;; CHECK: #f
(print (pair? "pair"))              ;; CHECK: #f
(print (pair? '()))                 ;; CHECK: #f
(print (pair? '(1 2 3)))            ;; CHECK: #t

(print (equal? 1 2))                ;; CHECK: #f
(print (equal? (+ 1 2) 3))          ;; CHECK: #t
(print (equal? #t #f))              ;; CHECK: #f
(print (equal? 'a 'b))              ;; CHECK: #f
(print (equal? "equal?" "equal?"))  ;; CHECK: #t
(print (equal? '() '()))            ;; CHECK: #t
(print (equal? '(1 + 2) '(1 + 2)))  ;; CHECK: #t
(print (equal? '(1 2 3) '(1 2)))    ;; CHECK: #f

(print (equal? '(a b c) '(a b c d)))                ;; CHECK: #f
(print (equal? '(a b (c)) (list 'a 'b (list 'c))))  ;; CHECK: #t

(define x (read "(lambda (x) (* x 100))"))
(print ((eval x) 3))

;; CHECK: 300

(define y '(* (length (list 'quoted 'list)) 200))
(print (eval y))

;; CHECK: 400

(print (read "(lambda () #t)"))
(print (read "(lambda () '#f)"))
(print (read "(define (a) 'b)"))
(print (read "(define (a x y) '(b))"))

;; CHECK: (lambda () #t)
;; CHECK: (lambda () (quote #f))
;; CHECK: (define a (lambda () (quote b)))
;; CHECK: (define a (lambda (x y) (quote (b))))

(print (string? (show 1)))
(print (string? (show #t)))
(print (string? (show "show")))
(print (string? (show (+ 1 2))))
(print (show (list 'a 'b 'c)))
(print (show (read "(define (a x y) '(b))")))

;; CHECK: #t
;; CHECK: #t
;; CHECK: #t
;; CHECK: #t
;; CHECK: (a b c)
;; CHECK: (define a (lambda (x y) (quote (b))))

(print (string? (string-append "abc")))
(print (string-append "foo" "bar"))
(print (string-append (show 1) (show '+) (show 2) (show '=) (show (+ 1 2))))
(print (string-append 2 '+ 3 '= (+ 2 3))) ; show is called implicitly
(print (string-append "List of arguments of length " (length argv) ": " argv))

;; CHECK: #t
;; CHECK: foobar
;; CHECK: 1+2=3
;; CHECK: 2+3=5
;; CHECK: List of arguments of length 1: (test/test.scm)

;; ----- Macros -----

(define-macro (infix a op b) `(,op ,a ,b))
(print (infix 2 - 1))

;; CHECK: 1

(define-macro (commute a op b) `(infix ,b ,op ,a))
(print (commute 2 - 1))

;; CHECK: -1

;; ----- "FFI" -----

(define lua "lua () return 1 end")
(print (lua))

;; CHECK: 1

(define lua "lua (a) return a + 1 end")
(print (lua 1))

;; CHECK: 2

(define lua "lua (a, b) return a + b end")
(print (lua 1 2))

;; CHECK: 3

;; ----- Prelude -----

(load "src/prelude.scm")

(define (fib n)
  (if (< n 2)
      n
      (+ (fib (- n 1)) (fib (- n 2)))))

(print (map fib (range 0 10 1)))

;; CHECK: (0 1 1 2 3 5 8 13 21 34 55)

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

;; CHECK: 42.000000378937

(define (countdown x)
  (lambda () (begin (set! x (- x 1)) x)))

(define c3 (countdown 3))

(print (begin (c3) (c3) (if (= (c3) 0) "Boom!")))

;; CHECK: Boom!

(define (plus x y)
  (let ((+ (lambda (x y)
             (if (and (list? x) (list? y))
                 (append x y)
                 (+ x y)))))
    (+ x y)))

(print (cons (plus 1 2) (plus '(4) '(5))))

;; CHECK: (3 4 5)

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

(print (even? 42))  ;; CHECK: #t
(print (odd? 42))   ;; CHECK: #f
(print (even? 43))  ;; CHECK: #f
(print (odd? 43))   ;; CHECK: #t

(let= loop ((i 0)
            (l '()))
  (if (<= i 10)
      (loop (+ i 1) (cons (fib i) l))
      (print l)))

;; CHECK: (55 34 21 13 8 5 3 2 1 1 0)

(let ((l '()))
  (begin
    (for i 0 10
         (set! l (cons (fib i) l)))
    (print l)))

;; CHECK: (55 34 21 13 8 5 3 2 1 1 0)

(define (sum-list lst)
  (let= loop ((acc 0)
              (l lst))
    (cond ((null? l) acc)
          (else (loop (+ acc (car l)) (cdr l))))))

(print (sum-list (range 1 100 1)))

;; CHECK: 5050

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

;; CHECK: (West)
