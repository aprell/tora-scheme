;; Built-in macros

(define-macro (and x y)
  `(if ,x ,y ,x)) ; equivalent to (list 'if x y x)

(define-macro (or x y)
  `(if ,x ,x ,y))

(define-macro (else x)
  `(#t ,x)) ; else-clause is always true
