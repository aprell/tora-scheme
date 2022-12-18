;; Built-in macros

(define-macro (and x y)
  `(if ,x ,y ,x)) ; equivalent to (list 'if x y x)

(define-macro (or x y)
  `(if ,x ,x ,y))

(define-macro (when test then)
  `(if ,test ,then))

(define-macro (unless test then)
  `(when (not ,test) ,then))

;; Named let defined in terms of letrec and lambda
(define-macro (let= name bindings body)
  (letrec (($cadr (lambda (lst) (car (cdr lst))))
           ($map (lambda (fn lst)
                  (if (null? lst) '()
                      (cons (fn (car lst)) ($map fn (cdr lst)))))))
    `(letrec ((,name (lambda ,(map car bindings) ,body)))
       (,name ,@($map $cadr bindings)))))

(define-macro (switch expr cases)
  (letrec (($test-equal (lambda (e pair)
                         (if (not (equal? `,(car pair) 'default))
                             `((equal? ,e ,(car pair)) ,@(cdr pair))
                             `(else ,@(cdr pair)))))
           ($map (lambda (fn lst)
                  (if (null? lst) '()
                      (cons (fn (car lst)) ($map fn (cdr lst)))))))
  `(let (($t ,expr))
     (cond ,@($map (lambda (pair) ($test-equal '$t pair)) cases)))))

(define-macro (while test body)
  `(let= $loop () (when ,test (begin ,body ($loop)))))

(define-macro (for i lo hi body)
  `(let ((,i ,lo))
     (while (<= ,i ,hi)
            (begin
              ,body
              (set! ,i (+ ,i 1))))))

(define-macro (assert-equal a b)
  `(if (not (equal? ,a ,b))
    (let (($msg (string-append
                 "Assertion failed: "
                 ,(show a) " != " ,(show b) " <=> " ,a " != " ,b)))
      (error $msg))))

(define-macro (assert-true a)
  `(assert-equal ,a #t))

(define-macro (assert-false a)
  `(assert-equal ,a #f))
