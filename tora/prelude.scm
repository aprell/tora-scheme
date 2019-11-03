(define add1 (lambda (x) (+ x 1)))

(define sub1 (lambda (x) (- x 1)))

(define abs (lambda (x) (if (< 0 x) x (- 0 x))))

(define square (lambda (x) (* x x)))

;; Little Schemer (Preface)
(define atom? (lambda (x)
  (and (not (pair? x)) (not (null? x)))))

;;(define atom? (lambda (x) (not (list? x))))

(define cadr (lambda (x) (car (cdr x))))
(define caar (lambda (x) (car (car x))))
(define cddr (lambda (x) (cdr (cdr x))))
(define cdar (lambda (x) (cdr (car x))))

(define append (lambda (l1 l2)
  (if (null? l1)
      l2
      (cons (car l1)
            (append (cdr l1) l2)))))

(define reverse (lambda (lst)
  (foldl (flip cons) '() lst)))

;; Higher-order functions

(define iter (lambda (fn lst)
  (if (not (null? lst))
      (begin
        (fn (car lst))
        (iter fn (cdr lst))))))

(define map (lambda (fn lst)
  (if (null? lst)
      '()
      (cons (fn (car lst))
            (map fn (cdr lst))))))

(define filter (lambda (p lst)
  (if (null? lst)
      '()
      (if (p (car lst))
          (cons (car lst) (filter p (cdr lst)))
          (filter p (cdr lst))))))

(define foldl (lambda (fn acc lst)
  (if (null? lst)
      acc
      (foldl fn (fn acc (car lst)) (cdr lst)))))

(define foldr (lambda (fn acc lst)
  (if (null? lst)
      acc
      (fn (car lst) (foldr fn acc (cdr lst))))))

(define reduce (lambda (fn lst)
  (foldl fn (car lst) (cdr lst))))

(define sum (lambda (lst)
  (reduce + lst)))

(define product (lambda (lst)
  (reduce * lst)))

(define min (lambda (lst)
  (reduce (lambda (x y) (if (< x y) x y)) lst)))

(define max (lambda (lst)
  (reduce (lambda (x y) (if (> x y) x y)) lst)))

(define compose (lambda (f g)
  (lambda (args) (f (g args)))))

(define flip (lambda (fn)
  (lambda (a b) (fn b a))))

(define (range start end step)
  (letrec ((up (lambda (start end step)
                 (if (> start end) '()
                     (cons start (up (+ start step) end step)))))
           (down (lambda (start end step)
                   (if (< start end) '()
                       (cons start (down (+ start step) end step))))))
    (cond ((and (<= start end) (> step 0)) (up start end step))
          ((and (>= start end) (< step 0)) (down start end step))
          (else '()))))
