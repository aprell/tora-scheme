(load "tora/prelude.scm")
(load "../common.scm")

(define unary-primitives
  '(add1 sub1 zero? box unbox car cdr))

(define binary-primitives
  '(+ - cons))

(define all-primitives
  `(,@unary-primitives ,@binary-primitives if let))

(define (variable? x)
  (and (symbol? x) (not (member? x all-primitives))))

(define unary-primitive?
  (unary? unary-primitives))

(define binary-primitive?
  (binary? binary-primitives))

(define (expr? expr)
  (cond
    ((number? expr) #t)
    ((boolean? expr) #t)
    ((equal? expr '()) #t)
    ((variable? expr) #t)

    ((unary-primitive? expr) (expr? (second expr)))

    ;; (+ e0 e1) | (- e0 e1) | (cons e0 e1)
    ((binary-primitive? expr)
     (let ((e0 (second expr))
           (e1 (third expr)))
       (and (expr? e0) (expr? e1))))

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
    ((equal? expr '()) (compile-empty expr))
    ((variable? expr) (compile-variable expr env))

    (((unary? 'add1) expr) (compile-add1 (second expr) env))
    (((unary? 'sub1) expr) (compile-sub1 (second expr) env))
    (((unary? 'zero?) expr) (compile-zero? (second expr) env))
    (((unary? 'box) expr) (compile-box (second expr) env))
    (((unary? 'unbox) expr) (compile-unbox (second expr) env))
    (((unary? 'car) expr) (compile-car (second expr) env))
    (((unary? 'cdr) expr) (compile-cdr (second expr) env))

    ;; (+ e0 e1)
    (((binary? '+) expr)
     (let ((e0 (second expr))
           (e1 (third expr)))
       (compile-binary 'add e0 e1 env)))

    ;; (- e0 e1)
    (((binary? '-) expr)
     (let ((e0 (second expr))
           (e1 (third expr)))
       (compile-binary 'sub e0 e1 env)))

    ;; (cons e0 e1)
    (((binary? 'cons) expr)
     (let ((e0 (second expr))
           (e1 (third expr)))
       (compile-cons e0 e1 env)))

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
  ;; Shift left by 5
  `((mov rax ,(* i 32))))

(define (compile-boolean b)
  ;; #t == 0b01000, #f == 0b10000
  `((mov rax ,(if b 8 16))))

(define (compile-empty e)
  ;; () == 0b11000
  `((mov rax 24)))

(define (compile-variable x env)
  (let ((i (lookup x env)))
    `((mov rax (offset rsp ,(- 0 (* i 8)))))))

(define (compile-add1 e0 env)
  (let ((c0 (compile/1 e0 env)))
    `(,@c0
       ,@assert-integer
       (add rax ,(* 1 32)))))

(define (compile-sub1 e0 env)
  (let ((c0 (compile/1 e0 env)))
    `(,@c0
       ,@assert-integer
       (sub rax ,(* 1 32)))))

(define (compile-zero? e0 env)
  (let ((c0 (compile/1 e0 env))
        (l0 (new-label "zero"))
        (l1 (new-label "zero")))
    `(,@c0
       ,@assert-integer
       (cmp rax 0)
       (jne ,l0)
       (mov rax 8)
       (jmp ,l1)
       (,l0)
       (mov rax 16)
       (,l1))))

(define (compile-box e0 env)
  (let ((c0 (compile/1 e0 env)))
    `(,@c0
       (mov (offset rdi 0) rax)
       (mov rax rdi)
       (_or rax 1) ;; prevent macro expansion
       (add rdi 8))))

(define (compile-unbox e0 env)
  (let ((c0 (compile/1 e0 env)))
    `(,@c0
       ,@assert-box
       (xor rax 1)
       (mov rax (offset rax 0)))))

(define (compile-car e0 env)
  (let ((c0 (compile/1 e0 env)))
    `(,@c0
       ,@assert-pair
       (xor rax 2)
       (mov rax (offset rax 0)))))

(define (compile-cdr e0 env)
  (let ((c0 (compile/1 e0 env)))
    `(,@c0
       ,@assert-pair
       (xor rax 2)
       (mov rax (offset rax 8)))))

;; (op e0 e1) => (let ((x e1)) (op e0 x))
(define (compile-binary op e0 e1 env)
  (let ((c1 (compile/1 e1 env))
        (c0 (compile/1 e0 (cons #f env))))
    `(,@c1
       ,@assert-integer
       (mov (offset rsp ,(- 0 (* (add1 (length env)) 8))) rax)
       ,@c0
       ,@assert-integer
       (,op rax (offset rsp ,(- 0 (* (add1 (length env)) 8)))))))

(define (compile-cons e0 e1 env)
  (let ((c1 (compile/1 e1 env))
        (c0 (compile/1 e0 (cons #f env))))
    `(,@c1
       (mov (offset rsp ,(- 0 (* (add1 (length env)) 8))) rax)
       ,@c0
       (mov (offset rdi 0) rax)
       (mov rax (offset rsp ,(- 0 (* (add1 (length env)) 8))))
       (mov (offset rdi 8) rax)
       (mov rax rdi)
       (_or rax 2) ;; prevent macro expansion
       (add rdi 16))))

(define (compile-if e0 e1 e2 env)
  (let ((c0 (compile/1 e0 env))
        (c1 (compile/1 e1 env))
        (c2 (compile/1 e2 env))
        (l0 (new-label "if"))
        (l1 (new-label "if")))
    `(,@c0
       (cmp rax 8)
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

(define (assert-type mask type)
  `((mov  rbx rax)
    (_and rbx ,mask) ;; prevent macro expansion
    (cmp  rbx ,type)
    (jne  err)))

;; 0b00000
(define assert-integer (assert-type (sub1 32) 0))

;; 0b001
(define assert-box (assert-type (sub1 8) 1))

;; 0b010
(define assert-pair (assert-type (sub1 8) 2))

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
            (('mov  (string-append tab "mov "  (operands->string operands)))
             ('add  (string-append tab "add "  (operands->string operands)))
             ('sub  (string-append tab "sub "  (operands->string operands)))
             ('cmp  (string-append tab "cmp "  (operands->string operands)))
             ('_and (string-append tab "and "  (operands->string operands)))
             ('_or  (string-append tab "or "   (operands->string operands)))
             ('xor  (string-append tab "xor "  (operands->string operands)))
             ('jmp  (string-append tab "jmp "  (label->string (first operands))))
             ('jne  (string-append tab "jne "  (label->string (first operands))))
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
