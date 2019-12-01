;; Without pattern matching, we rely on car and cdr to deconstruct lists

(define first car)
(define second cadr)

(define n-th
  (lambda (lst n)
    (if (= n 0)
        (car lst)
        (n-th (cdr lst) (sub1 n)))))

(define (n-ary? n)
  (lambda (expr)
    (and (list? expr) (= (length expr) (add1 n)))))

(define (unary? op)
  (lambda (expr)
    (and ((n-ary? 1) expr) (equal? (first expr) op))))

(define (binary? op)
  (lambda (expr)
    (and ((n-ary? 2) expr) (equal? (first expr) op))))

(define (ternary? op)
  (lambda (expr)
    (and ((n-ary? 3) expr) (equal? (first expr) op))))

(define format
  "lua (n) return ('%03d'):format(n) end")

(define (gensym n)
  (lambda (str)
    (let ((label (string-append "L_" str "_" (format n))))
      (begin
        (set! n (add1 n))
        label))))
