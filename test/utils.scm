(load "src/prelude.scm")

(define fail 0)
(define (fail!)
  (set! fail (add1 fail)))

(define pass 0)
(define (pass!)
  (set! pass (add1 pass)))

(define (total)
  (+ fail pass))

(define-macro (test a b)
  `(if (not (equal? ,a ,b))
    (let (($msg (string-append
                  "Test " (add1 (total)) " failed: "
                  ,(show a) " != " ,(show b) " <=> " ,a " != " ,b)))
      (begin
        (print $msg)
        (fail!)))
    (pass!)))

(define-macro (test/exn a e)
  `(test (eval/exn ',a) ,e))

(define-macro (test/true a)
  `(test ,a #t))

(define-macro (test/false a)
  `(test ,a #f))

(define-macro (test/nil a)
  `(test ,a $nil))

(define (test/result)
  (if (> fail 0)
      (print (string-append fail "/" (total) " tests failed"))
      (print (string-append "All " (total) " tests passed"))))
