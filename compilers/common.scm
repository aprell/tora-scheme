;; Without pattern matching, we rely on car and cdr to deconstruct lists

(define first car)

(define rest cdr)

(define second cadr)

(define third caddr)

(define (fourth lst)
  (first (cdddr lst)))

(define (fifth lst)
  (second (cdddr lst)))

(define n-th
  (lambda (lst n)
    (if (= n 0)
        (car lst)
        (n-th (cdr lst) (sub1 n)))))

(define (n-ary? n)
  (lambda (expr)
    (and (list? expr) (= (length expr) (add1 n)))))

(define (unary? op)
  (let ((cmp (if (list? op) member? equal?)))
    (lambda (expr)
      (and ((n-ary? 1) expr) (cmp (first expr) op)))))

(define (binary? op)
  (let ((cmp (if (list? op) member? equal?)))
    (lambda (expr)
      (and ((n-ary? 2) expr) (cmp (first expr) op)))))

(define (ternary? op)
  (let ((cmp (if (list? op) member? equal?)))
    (lambda (expr)
      (and ((n-ary? 3) expr) (cmp (first expr) op)))))

(define format
  "lua (n) return ('%03d'):format(n) end")

(define (gensym n)
  (lambda (str)
    (let ((label (string-append "L_" str "_" (format n))))
      (begin
        (set! n (add1 n))
        label))))

(define (unimplemented name)
  (error (string-append "Unimplemented: " name)))
