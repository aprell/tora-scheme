(load "tora/prelude.scm")
(load "../common.scm")

(define (variable? x)
  (and (symbol? x) (not (member? x '(add1 sub1 zero? if let)))))

(define (expr? expr)
  (cond
    ((number? expr) #t)
    ((boolean? expr) #t)
    ((variable? expr) #t)

    (((unary? 'add1) expr) (expr? (second expr)))
    (((unary? 'sub1) expr) (expr? (second expr)))
    (((unary? 'zero?) expr) (expr? (second expr)))

    ;; (if e0 e1 e2)
    (((ternary? 'if) expr)
     (let ((e0 (second expr))
           (e1 (third expr))
           (e2 (fourth expr)))
       (and (expr? e0) (and (expr? e1) (expr? e2)))))

    ;; (let ((id e0)) e1)
    (((binary? 'let) expr)
     (let ((id (caar (second expr)))
           (e0 (cadar (second expr)))
           (e1 (third expr)))
       (and (variable? id) (and (expr? e0) (expr? e1)))))

    (else #f)))

(define new-label (gensym 0))

(define (compile expr)
  `((entry)
    ,@(compile/1 expr '())
    (ret)
    (err)
    (push rbp)
    (call error)))

(define (compile/1 expr env)
  (cond
    ((number? expr) (compile-integer expr))
    ((boolean? expr) (compile-boolean expr))
    ((variable? expr) (compile-variable expr env))

    (((unary? 'add1) expr) (compile-add1 (second expr) env))
    (((unary? 'sub1) expr) (compile-sub1 (second expr) env))
    (((unary? 'zero?) expr) (compile-zero? (second expr) env))

    ;; (if e0 e1 e2)
    (((ternary? 'if) expr)
     (let ((e0 (second expr))
           (e1 (third expr))
           (e2 (fourth expr)))
       (compile-if e0 e1 e2 env)))

    ;; (let ((id e0)) e1)
    (((binary? 'let) expr)
     (let ((id (caar (second expr)))
           (e0 (cadar (second expr)))
           (e1 (third expr)))
       (compile-let id e0 e1 env)))

    (else (error "compile/1"))))

(define (compile-integer i)
  `((mov rax ,(* i 2))))

(define (compile-boolean b)
  `((mov rax ,(if b 3 1))))

(define (compile-variable x env)
  (let ((i (lookup x env)))
    `((mov rax (offset rsp ,(- 0 (* i 8)))))))

(define (compile-add1 e0 env)
  (let ((c0 (compile/1 e0 env)))
    `(,@c0
       ,@assert-integer
       (add rax ,(* 1 2)))))

(define (compile-sub1 e0 env)
  (let ((c0 (compile/1 e0 env)))
    `(,@c0
       ,@assert-integer
       (sub rax ,(* 1 2)))))

(define (compile-zero? e0 env)
  (let ((c0 (compile/1 e0 env))
        (l0 (new-label "zero"))
        (l1 (new-label "zero")))
    `(,@c0
       ,@assert-integer
       (cmp rax 0)
       (jne ,l0)
       (mov rax 3)
       (jmp ,l1)
       (,l0)
       (mov rax 1)
       (,l1))))

(define (compile-if e0 e1 e2 env)
  (let ((c0 (compile/1 e0 env))
        (c1 (compile/1 e1 env))
        (c2 (compile/1 e2 env))
        (l0 (new-label "if"))
        (l1 (new-label "if")))
    `(,@c0
       (cmp rax 3)
       (jne ,l0)
       ,@c1
       (jmp ,l1)
       (,l0)
       ,@c2
       (,l1))))

(define (compile-let x e0 e1 env)
  (let ((c0 (compile/1 e0 env))
        (c1 (compile/1 e1 (cons x env))))
    `(,@c0
       (mov (offset rsp ,(- 0 (* (add1 (length env)) 8))) rax)
       ,@c1)))

(define (lookup x env)
  (let ((res (member x env)))
    (if (list? res)
        (length res)
        (error (string-append "Undefined variable " x)))))

(assert-equal (lookup 'x '(a b x)) 1)
(assert-equal (lookup 'x '(a x c)) 2)
(assert-equal (lookup 'x '(x b c)) 3)
(assert-equal (lookup 'x '(x x x)) 3)

(define assert-integer
  `((mov  rbx rax)
    (_and rbx 1) ;; prevent macro expansion
    (cmp  rbx 0)
    (jne  err)))

;; Prefix label with an underscore (macOS)
(define (label->string label)
  (string-append "_" label))

(define (operands->string operands)
  (let ((op1 (first operands))
        (op2 (second operands)))
    (string-append
      (if ((binary? 'offset) op1)
          ;; [Register + Integer]
          (string-append "[" (second op1) " + " (third op1) "]")
          op1)
      ", "
      (if ((binary? 'offset) op2)
          ;; [Register + Integer]
          (string-append "[" (second op2) " + " (third op2) "]")
          op2))))

(define (instr->string instr)
  (let ((opcode   (car instr))
        (operands (cdr instr)))
    (switch opcode
            (('mov  (string-append tab "mov " (operands->string operands)))
             ('add  (string-append tab "add " (operands->string operands)))
             ('sub  (string-append tab "sub " (operands->string operands)))
             ('cmp  (string-append tab "cmp " (operands->string operands)))
             ('_and (string-append tab "and " (operands->string operands)))
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
