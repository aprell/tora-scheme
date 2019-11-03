;; ---------------------------------------------------------------------------
;;  Code transcribed from the article
;;  Programming Language Theory Explained for the Working Programmer:
;;  Principles of Programming Languages
;; ---------------------------------------------------------------------------
;;  https://www.leafac.com/prose/programming-language-theory-explained-for-the-working-programmer--principles-of-programming-languages
;; ---------------------------------------------------------------------------

(load "tora/prelude.scm")

;; STARTING POINT

(define (zero? number)
  (= 0 number))

(define (sum-up-to number)
  (if (zero? number)
    0
    (+ number (sum-up-to (sub1 number)))))

(assert-equal (sum-up-to 5) 15)

;; NUMBERS

(define (zero  fun arg) arg)
(define (one   fun arg) (fun arg))
(define (two   fun arg) (fun (fun arg)))
(define (three fun arg) (fun (fun (fun arg))))
(define (four  fun arg) (fun (fun (fun (fun arg)))))
(define (five  fun arg) (fun (fun (fun (fun (fun arg))))))

(define (pretty-print number)
  (number add1 0))

(assert-equal (pretty-print five) 5)

(define (zero? number)
  (let ((always-false
          (lambda (_) #f)))
    (number always-false #t)))

(assert-true (zero? zero))
(assert-false (zero? one))
(assert-false (zero? two))

;; Don't redefine '+' because add1 relies on it
(define (plus number-left number-right)
  (lambda (fun arg)
    (number-left fun (number-right fun arg))))

(assert-equal (pretty-print (plus zero zero)) 0)
(assert-equal (pretty-print (plus zero one)) 1)
(assert-equal (pretty-print (plus one two)) 3)

(define (sub1 number)
  (letrec ((initial-pair
             (list zero zero))
           (slide-pair
             (lambda (current-pair)
               (let ((current-number (cadr current-pair)))
                 (list current-number (plus current-number one)))))
           (final-pair
             (number slide-pair initial-pair)))
    (car final-pair)))

(assert-equal (pretty-print (sub1 one)) 0)
(assert-equal (pretty-print (sub1 two)) 1)
(assert-equal (pretty-print (sub1 three)) 2)

(define (sum-up-to number)
  (if (zero? number)
    zero
    (plus number (sum-up-to (sub1 number)))))

(assert-equal (pretty-print (sum-up-to five)) 15)

;; BOOLEANS

(define (true  first second) (first))
(define (false first second) (second))

(define (zero? number)
  (let ((always-false
          (lambda (_) false)))
    (number always-false true)))

(define (if_ test then els)
  (test then els))

(define (sum-up-to number)
  (if_ (zero? number)
    (lambda () zero)
    (lambda () (plus number (sum-up-to (sub1 number))))))

(assert-equal (pretty-print (sum-up-to five)) 15)

;; PAIRS

(define (store value)
  (lambda () value))

(define stored-5 (store five))
(define stored-1 (store one))

(assert-equal (pretty-print (stored-5)) 5)
(assert-equal (pretty-print (stored-1)) 1)

(define (pair left right)
  (lambda (selector)
    (selector left right)))

(define (pair-left pair)
  (let ((select-left
          (lambda (left right) left)))
    (pair select-left)))

(define (pair-right pair)
  (let ((select-right
          (lambda (left right) right)))
    (pair select-right)))

(assert-equal (pretty-print (pair-left (pair two three))) 2)
(assert-equal (pretty-print (pair-right (pair two three))) 3)

(define (sub1 number)
  (letrec ((initial-pair
             (pair zero zero))
           (slide-pair
             (lambda (current-pair)
               (let ((current-number (pair-right current-pair)))
                 (pair current-number (plus current-number one)))))
           (final-pair
             (number slide-pair initial-pair)))
    (pair-left final-pair)))

(assert-equal (pretty-print (sum-up-to five)) 15)

;; RECURSION

(define sum-up-to/rest "Tying the knot later")

(define (sum-up-to number)
  (if_ (zero? number)
    (lambda () zero)
    (lambda () (plus number (sum-up-to/rest (sub1 number))))))

(set! sum-up-to/rest sum-up-to)

(assert-equal (pretty-print (sum-up-to five)) 15)

(define (sum-up-to number)
  (let ((sum-up-to/partial
          (lambda (sum-up-to/rest number)
            (if_ (zero? number)
                 (lambda () zero)
                 (lambda () (plus number (sum-up-to/rest sum-up-to/rest (sub1 number))))))))
    (sum-up-to/partial sum-up-to/partial number)))

(assert-equal (pretty-print (sum-up-to five)) 15)

;; FUNCTIONS WITH MULTIPLE ARGUMENTS

(define (zero  fun) (lambda (arg) arg))
(define (one   fun) (lambda (arg) (fun arg)))
(define (two   fun) (lambda (arg) (fun (fun arg))))
(define (three fun) (lambda (arg) (fun (fun (fun arg)))))
(define (four  fun) (lambda (arg) (fun (fun (fun (fun arg))))))
(define (five  fun) (lambda (arg) (fun (fun (fun (fun (fun arg)))))))

(define (pretty-print number)
  ((number add1) 0))

;; Don't redefine '+' because add1 relies on it
(define (plus number-left)
  (lambda (number-right)
    (lambda (fun)
      (lambda (arg)
      ((number-left fun) ((number-right fun) arg))))))

(define (true  first) (lambda (second) (first)))
(define (false first) (lambda (second) (second)))

(define (zero? number)
  (let ((always-false
          (lambda (_) false)))
    ((number always-false) true)))

(define (if_ test)
  (lambda (then)
    (lambda (els)
      ((test then) els))))

(assert-equal (pretty-print ((plus zero) zero)) 0)
(assert-equal (pretty-print ((plus zero) one)) 1)
(assert-equal (pretty-print ((plus one) two)) 3)

(define (pair left)
  (lambda (right)
    (lambda (selector)
      ((selector left) right))))

(define (pair-left pair)
  (let ((select-left
          (lambda (left)
            (lambda (right) left))))
    (pair select-left)))

(define (pair-right pair)
  (let ((select-right
          (lambda (left)
            (lambda (right) right))))
    (pair select-right)))

(assert-equal (pretty-print (pair-left ((pair two) three))) 2)
(assert-equal (pretty-print (pair-right ((pair two) three))) 3)

(define (sub1 number)
  (letrec ((initial-pair
             ((pair zero) zero))
           (slide-pair
             (lambda (current-pair)
               (let ((current-number (pair-right current-pair)))
                 ((pair current-number) ((plus current-number) one)))))
           (final-pair
             ((number slide-pair) initial-pair)))
    (pair-left final-pair)))

(assert-equal (pretty-print (sub1 one)) 0)
(assert-equal (pretty-print (sub1 two)) 1)
(assert-equal (pretty-print (sub1 three)) 2)

(define (sum-up-to number)
  (let ((sum-up-to/partial
          (lambda (sum-up-to/rest)
            (lambda (number)
              (((if_ (zero? number))
                   (lambda () zero))
                   (lambda () ((plus number) ((sum-up-to/rest sum-up-to/rest) (sub1 number)))))))))
    ((sum-up-to/partial sum-up-to/partial) number)))

(assert-equal (pretty-print (sum-up-to five)) 15)

(print "All tests passed")
