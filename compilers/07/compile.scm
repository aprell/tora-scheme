(load "src/prelude.scm")
(load "../common.scm")

(define unary-primitive? (unary? '(add1 sub1 zero?)))

(define (expr? expr)
  (cond
    ((number? expr) #t)
    ((boolean? expr) #t)
    ((unary-primitive? expr) (expr? (second expr)))
    ;; (if e0 e1 e2)
    (((ternary? 'if) expr)
     (let ((e0 (second expr))
           (e1 (third expr))
           (e2 (fourth expr)))
       (and (expr? e0) (and (expr? e1) (expr? e2)))))
    (else #f)))

(define (bits->value bs)
  (if (even? bs) ;; Least-significant bit of 0 => Integer
      (/ bs 2)
      (switch bs ;; Least-significant bit of 1 => Boolean
              ((1 #f)
               (3 #t)))))

(define (value->bits v)
  (switch v
          ((#f 1)
           (#t 3)
           (default ;; Integer
             (* v 2)))))

(define new-label (gensym 0))

(define (compile expr)
  `((entry)
    ,@(compile/1 expr)
    (ret)
    (err)
    (push rbp)
    (call error)))

(define (compile/1 expr)
  (cond
    ((number? expr) `((mov rax ,(value->bits expr))))
    ((boolean? expr) `((mov rax ,(if expr (value->bits #t) (value->bits #f)))))

    (((unary? 'add1) expr)
     (let ((c0 (compile/1 (second expr))))
       `(,@c0
          ,@assert-integer
          (add rax ,(value->bits 1)))))

    (((unary? 'sub1) expr)
     (let ((c0 (compile/1 (second expr))))
       `(,@c0
          ,@assert-integer
          (sub rax ,(value->bits 1)))))

    (((unary? 'zero?) expr)
     (let ((c0 (compile/1 (second expr)))
           (l0 (new-label "zero"))
           (l1 (new-label "zero")))
       `(,@c0
          ,@assert-integer
          (cmp rax 0)
          (jne ,l0)
          (mov rax ,(value->bits #t))
          (jmp ,l1)
          (,l0)
          (mov rax ,(value->bits #f))
          (,l1))))

    (((ternary? 'if) expr)
     (let ((c0 (compile/1 (second expr)))
           (c1 (compile/1 (third expr)))
           (c2 (compile/1 (fourth expr)))
           (l0 (new-label "if"))
           (l1 (new-label "if")))
       `(,@c0
          (cmp rax ,(value->bits #t))
          (jne ,l0)
          ,@c1
          (jmp ,l1)
          (,l0)
          ,@c2
          (,l1))))

    (else (error "compile/1"))))

(define assert-integer
  `((mov  rbx rax)
    (_and rbx 1) ;; prevent macro expansion
    (cmp  rbx 0)
    (jne  err)))

;; Prefix label with an underscore (macOS)
(define (label->string label)
  (string-append "_" label))

(define (instr->string instr)
  (let ((opcode   (car instr))
        (operands (cdr instr)))
    (switch opcode
            (('mov  (string-append tab "mov " (first operands) ", " (second operands)))
             ('add  (string-append tab "add " (first operands) ", " (second operands)))
             ('sub  (string-append tab "sub " (first operands) ", " (second operands)))
             ('cmp  (string-append tab "cmp " (first operands) ", " (second operands)))
             ('_and (string-append tab "and " (first operands) ", " (second operands)))
             ('jmp  (string-append tab "jmp " (label->string (first operands))))
             ('jne  (string-append tab "jne " (label->string (first operands))))
             ('call (string-append tab "call " (label->string (first operands))))
             ('push (string-append tab "push " (first operands)))
             ('ret  (string-append tab "ret"))
             (default ;; Label
               (string-append (label->string opcode) ":"))))))

(define (emit instrs)
  (let ((entry (caar instrs)))
    (begin
      (print (string-append tab "global " (label->string entry)))
      (print (string-append tab "extern " (label->string 'error)))
      (print (string-append tab "section .text"))
      (iter (compose print instr->string) instrs))))

(define (main)
  (begin
    (if (<= (length argv) 1)
        (error (string-append "Filename expected: arguments are " (cdr argv))))
    (let ((filename (second argv)))
      (let ((input (read-file filename)))
        (if (expr? input)
            (emit (compile input))
            (error (string-append "Syntax error: " input)))))))

(main)
