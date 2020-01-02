(load "tora/prelude.scm")
(load "../common.scm")

(define unary-primitives
  '(add1 sub1 zero? box unbox car cdr))

(define binary-primitives
  '(+ - cons))

(define all-primitives
  `(,@unary-primitives ,@binary-primitives if let))

(define (immediate? expr)
  (or (number? expr) (or (boolean? expr) (equal? expr '()))))

(define (variable? x)
  (and (symbol? x) (not (member? x all-primitives))))

(define unary-primitive?
  (unary? unary-primitives))

(define binary-primitive?
  (binary? binary-primitives))

(define (expr? expr)
  (cond
    ((immediate? expr) #t)
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

    ;; (lambda (x ...) e0)
    (((binary? 'lambda) expr)
     (let ((xs (second expr))
           (e0 (third expr)))
       (and (all variable? xs) (expr? e0))))

    (else
      ;; Function application: (e0 ...)
      (and (expr? (first expr)) (all expr? (rest expr))))))

(define new-label (gensym 0))

(define (label-lambdas expr)
  (cond
    ((immediate? expr) expr)
    ((variable? expr) expr)

    ((unary-primitive? expr)
     (let ((op (first expr))
           (e0 (second expr)))
       `(,op ,(label-lambdas e0))))

    ;; (+ e0 e1) | (- e0 e1) | (cons e0 e1)
    ((binary-primitive? expr)
     (let ((op (first expr))
           (e0 (second expr))
           (e1 (third expr)))
       `(,op ,(label-lambdas e0) ,(label-lambdas e1))))

    ;; (if e0 e1 e2)
    (((ternary? 'if) expr)
     (let ((e0 (second expr))
           (e1 (third expr))
           (e2 (fourth expr)))
       `(if ,(label-lambdas e0) ,(label-lambdas e1) ,(label-lambdas e2))))

    ;; (let ((id e0)) e1)
    (((binary? 'let) expr)
     (let ((id (caar (second expr)))
           (e0 (cadar (second expr)))
           (e1 (third expr)))
       `(let ((,id ,(label-lambdas e0))) ,(label-lambdas e1))))

    ;; (lambda (x ...) e0)
    (((binary? 'lambda) expr)
     (let ((xs (second expr))
           (e0 (third expr)))
       `(lambda ,(new-label "fun") ,xs ,(label-lambdas e0))))

    (else
      ;; Function application: (e0 ...)
      (let ((e0 (first expr))
            (es (rest expr)))
        `(,(label-lambdas e0) ,@(map label-lambdas es))))))

;; expr is a labeled expression
(define (collect-lambdas expr)
  (cond
    ((immediate? expr) '())
    ((variable? expr) '())

    ((unary-primitive? expr)
     (let ((op (first expr))
           (e0 (second expr)))
       (collect-lambdas e0)))

    ;; (+ e0 e1) | (- e0 e1) | (cons e0 e1)
    ((binary-primitive? expr)
     (let ((op (first expr))
           (e0 (second expr))
           (e1 (third expr)))
       (append (collect-lambdas e0)
               (collect-lambdas e1))))

    ;; (if e0 e1 e2)
    (((ternary? 'if) expr)
     (let ((e0 (second expr))
           (e1 (third expr))
           (e2 (fourth expr)))
       (append (collect-lambdas e0)
               (append (collect-lambdas e1)
                       (collect-lambdas e2)))))

    ;; (let ((id e0)) e1)
    (((binary? 'let) expr)
     (let ((id (caar (second expr)))
           (e0 (cadar (second expr)))
           (e1 (third expr)))
       (append (collect-lambdas e0)
               (collect-lambdas e1))))

    ;; (lambda f (x ...) e0)
    (((ternary? 'lambda) expr)
     (let ((f (second expr))
           (xs (third expr))
           (e0 (fourth expr)))
       (cons expr (collect-lambdas e0))))

    (else
      ;; Function application: (e0 ...)
      (let ((e0 (first expr))
            (es (rest expr)))
        (append (collect-lambdas e0)
                (foldl append '() (map collect-lambdas es)))))))

;; expr is a labeled expression
(define (free-variables expr)
  (begin
    (define (free-variables expr)
      (cond
        ((immediate? expr) '())
        ((variable? expr) (list expr))

        ((unary-primitive? expr)
         (let ((op (first expr))
               (e0 (second expr)))
           (free-variables e0)))

        ;; (+ e0 e1) | (- e0 e1) | (cons e0 e1)
        ((binary-primitive? expr)
         (let ((op (first expr))
               (e0 (second expr))
               (e1 (third expr)))
           (append (free-variables e0)
                   (free-variables e1))))

        ;; (if e0 e1 e2)
        (((ternary? 'if) expr)
         (let ((e0 (second expr))
               (e1 (third expr))
               (e2 (fourth expr)))
           (append (free-variables e0)
                   (append (free-variables e1)
                           (free-variables e2)))))

        ;; (let ((id e0)) e1)
        (((binary? 'let) expr)
         (let ((id (caar (second expr)))
               (e0 (cadar (second expr)))
               (e1 (third expr)))
           (append (free-variables e0)
                   (remove id (free-variables e1)))))

        ;; (lambda f (x ...) e0)
        (((ternary? 'lambda) expr)
         (let ((f (second expr))
               (xs (third expr))
               (e0 (fourth expr)))
           (foldl (lambda (acc x) (remove x acc)) (free-variables e0) xs)))

        (else
          ;; Function application: (e0 ...)
          (let ((e0 (first expr))
                (es (rest expr)))
            (append (free-variables e0)
                    (foldl append '() (map free-variables es)))))))

    (remove-duplicates (free-variables expr))))

(define (compile expr)
  (let ((labeled-expr (label-lambdas expr)))
    (let ((lambdas (collect-lambdas labeled-expr)))
      `((label entry)
        ,@(compile-tail/1 labeled-expr '())
        (ret)
        ,@(compile-lambda-definitions lambdas)
        (label err)
        (push rbp)
        (call (label error))))))

;; Compile labeled expr in non-tail position
(define (compile/1 expr env)
  (cond
    ((immediate? expr) (compile-immediate expr))
    ((variable? expr) (compile-variable expr env))

    ((unary-primitive? expr)
     (let ((op (first expr))
           (e0 (second expr)))
       (compile-unary op e0 env)))

    ;; (+ e0 e1) | (- e0 e1) | (cons e0 e1)
    ((binary-primitive? expr)
     (let ((op (first expr))
           (e0 (second expr))
           (e1 (third expr)))
       (compile-binary op e0 e1 env)))

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

    ;; (lambda f (x ...) e0)
    (((ternary? 'lambda) expr)
     (let ((f (second expr))
           (xs (third expr))
           (e0 (fourth expr)))
       (compile-lambda f xs (free-variables expr) env)))

    (else
      ;; Function application: (e0 ...)
      (let ((e0 (first expr))
            (es (rest expr)))
        (compile-call e0 es env)))))

;; Compile labeled expr in tail position
(define (compile-tail/1 expr env)
  (cond
    ((immediate? expr) (compile-immediate expr))
    ((variable? expr) (compile-variable expr env))

    ((unary-primitive? expr)
     (let ((op (first expr))
           (e0 (second expr)))
       (compile-unary op e0 env)))

    ;; (+ e0 e1) | (- e0 e1) | (cons e0 e1)
    ((binary-primitive? expr)
     (let ((op (first expr))
           (e0 (second expr))
           (e1 (third expr)))
       (compile-binary op e0 e1 env)))

    ;; (if e0 e1 e2)
    (((ternary? 'if) expr)
     (let ((e0 (second expr))
           (e1 (third expr))
           (e2 (fourth expr)))
       (compile-tail-if e0 e1 e2 env)))

    ;; (let ((id e0)) e1)
    (((binary? 'let) expr)
     (let ((id (caar (second expr)))
           (e0 (cadar (second expr)))
           (e1 (third expr)))
       (compile-tail-let id e0 e1 env)))

    ;; (lambda f (x ...) e0)
    (((ternary? 'lambda) expr)
     (let ((f (second expr))
           (xs (third expr))
           (e0 (fourth expr)))
       (compile-lambda f xs (free-variables expr) env)))

    (else
      ;; Function application: (e0 ...)
      (let ((e0 (first expr))
            (es (rest expr)))
        (compile-tail-call e0 es env)))))

(define (compile-immediate i)
  `((mov rax
         ,(cond
            ((number? i) (* i 32))     ;; Shift left by 5
            ((boolean? i) (if i 8 16)) ;; #t == 0b01000, #f == 0b10000
            ((equal? i '()) 24)))))    ;; () == 0b11000

(define (compile-variable x env)
  (let ((i (lookup x env)))
    `((mov rax (offset rsp ,(- 0 (* i 8)))))))

(define (compile-unary op e0 env)
  ((symbol-append 'compile- op) e0 env))

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
       (jne (label ,l0))
       (mov rax 8)
       (jmp (label ,l1))
       (label ,l0)
       (mov rax 16)
       (label ,l1))))

(define type-imm  0) ;; 0b000
(define type-box  1) ;; 0b001
(define type-pair 2) ;; 0b010
(define type-fun  3) ;; 0b011

(define (compile-box e0 env)
  (let ((c0 (compile/1 e0 env)))
    `(,@c0
       (mov (offset rdi 0) rax)
       (mov rax rdi)
       ;; Tag pointer as box
       (_or rax ,type-box)
       (add rdi 8))))

(define (compile-unbox e0 env)
  (let ((c0 (compile/1 e0 env)))
    `(,@c0
       ,@assert-box
       ;; Untag pointer
       (xor rax ,type-box)
       (mov rax (offset rax 0)))))

(define (compile-car e0 env)
  (let ((c0 (compile/1 e0 env)))
    `(,@c0
       ,@assert-pair
       ;; Untag pointer
       (xor rax ,type-pair)
       (mov rax (offset rax 0)))))

(define (compile-cdr e0 env)
  (let ((c0 (compile/1 e0 env)))
    `(,@c0
       ,@assert-pair
       ;; Untag pointer
       (xor rax ,type-pair)
       (mov rax (offset rax 8)))))

;; (op e0 e1) => (let ((x e1)) (op e0 x))
(define (compile-binary op e0 e1 env)
  (let ((c1 (compile/1 e1 env))
        (c0 (compile/1 e0 (cons #f env)))
        (i (- 0 (* (add1 (length env)) 8))))
    (if (member? op '(+ -))
      ;; (+ e0 e1) | (- e0 e1)
      `(,@c1
         ,@assert-integer
         (mov (offset rsp ,i) rax)
         ,@c0
         ,@assert-integer
         (,(switch op (('+ 'add) ('- 'sub)))
           rax (offset rsp ,i)))
      ;; (cons e0 e1)
      `(,@c1
         (mov (offset rsp ,i) rax)
         ,@c0
         (mov (offset rdi 0) rax)
         (mov rax (offset rsp ,i))
         (mov (offset rdi 8) rax)
         (mov rax rdi)
         ;; Tag pointer as pair
         (_or rax ,type-pair)
         (add rdi 16)))))

(define (compile-if e0 e1 e2 env)
  (let ((c0 (compile/1 e0 env))
        (c1 (compile/1 e1 env))
        (c2 (compile/1 e2 env))
        (l0 (new-label "if"))
        (l1 (new-label "if")))
    `(,@c0
       (cmp rax 8)
       (jne (label ,l0))
       ,@c1
       (jmp (label ,l1))
       (label ,l0)
       ,@c2
       (label ,l1))))

(define (compile-tail-if e0 e1 e2 env)
  (let ((c0 (compile/1 e0 env))
        (c1 (compile-tail/1 e1 env))
        (c2 (compile-tail/1 e2 env))
        (l0 (new-label "if"))
        (l1 (new-label "if")))
    `(,@c0
       (cmp rax 8)
       (jne (label ,l0))
       ,@c1
       (jmp (label ,l1))
       (label ,l0)
       ,@c2
       (label ,l1))))

(define (compile-let x e0 e1 env)
  (let ((c0 (compile/1 e0 env))
        (c1 (compile/1 e1 (cons x env)))
        (i (- 0 (* (add1 (length env)) 8))))
    `(,@c0
       (mov (offset rsp ,i) rax)
       ,@c1)))

(define (compile-tail-let x e0 e1 env)
  (let ((c0 (compile/1 e0 env))
        (c1 (compile-tail/1 e1 (cons x env)))
        (i (- 0 (* (add1 (length env)) 8))))
    `(,@c0
       (mov (offset rsp ,i) rax)
       ,@c1)))

(define (compile-lambda-definitions lst)
  (foldl append '() (map compile-lambda-definition lst)))

(define (compile-lambda-definition expr)
  (let ((f (second expr))
        (xs (third expr))
        (e0 (fourth expr)))
    (let ((c0 (compile-tail/1 e0 (reverse (append xs (free-variables expr))))))
      `((label ,f) ;; TODO: (symbol->label f)
        ,@c0
        (ret)))))

;; Closure layout:
;;
;; struct closure {
;;     void *f;
;;     long n;
;;     long v[n];
;; };
;;
(define (compile-lambda f xs ys env)
  `(;; Save function pointer
    (lea rax (offset ,f 0))
    (mov (offset rdi 0) rax)
    ;; Save number of free variables
    (mov r8 ,(length ys))
    (mov (offset rdi 8) r8)
    (mov r9 rdi)
    (add r9 16)
    ;; Capture free variables
    ,@(copy-env-to-heap ys env)
    ;; Return closure pointer
    (mov rax rdi)
    ;; Tag pointer as function
    (_or rax ,type-fun)
    (add rdi ,(* (+ (length ys) 2) 8))))

(define (copy-env-to-heap ys env)
  (if (null? ys) '()
    (let ((i (lookup (first ys) env)))
      `((mov r8 (offset rsp ,(- 0 (* i 8))))
        ;; r9 points to the next free heap location
        (mov (offset r9 0) r8)
        (add r9 8)
        ,@(copy-env-to-heap (rest ys) env)))))

;; Compile a function call in non-tail position
(define (compile-call e0 es env)
  (let ((c0 (compile/1 e0 env))
        (cs (compile-args es (cons #f env)))
        (sz (* (length env) 8))
        (i (- 0 (* (add1 (length env)) 8))))
    `(,@c0
       (mov (offset rsp ,i) rax)
       ,@cs
       (mov rax (offset rsp ,i))
       ,@assert-fun
       ;; Untag pointer
       (xor rax ,type-fun)
       (sub rsp ,sz)
       ;; rcx := rsp - (n + 2) * 8
       (mov rcx rsp)
       (sub rcx ,(* (+ (length es) 2) 8))
       ,@(copy-closure-env-to-stack)
       (call (offset rax 0))
       (add rsp ,sz))))

;; Compile a function call in tail position
(define (compile-tail-call e0 es env)
  (let ((c0 (compile/1 e0 env))
        (cs (compile-args es (cons #f env)))
        (i (- 0 (* (add1 (length env)) 8))))
    `(,@c0
       (mov (offset rsp ,i) rax)
       ,@cs
       (mov rax (offset rsp ,i))
       ,@(move-args (length es) (add1 (length env)))
       ,@assert-fun
       ;; Untag pointer
       (xor rax ,type-fun)
       ;; rcx := rsp - (n + 1) * 8
       (mov rcx rsp)
       (sub rcx ,(* (+ (length es) 1) 8))
       ,@(copy-closure-env-to-stack)
       (jmp (offset rax 0)))))

;; Copy closure's environment from heap to stack
;; (shows stack for compile-call with free slot for return address)
;;
;;         STACK                        HEAP
;;     +-----------+ <-- rsp        +-----------+ <-- rax
;;     |           |                |     f     |
;;     +-----------+                +-----------+
;;     |     f     |                |     n     |
;;     +-----------+                +-----------+ <-- r9
;;     | explicit  |                | implicit  |
;;     ~~~~~~~~~~~~~                ~~~~~~~~~~~~~
;;     | arguments |                | arguments |
;;     +-----------+ <-- rcx        +-----------+
;;     | implicit  |
;;     ~~~~~~~~~~~~~
;;     | arguments |
;;     +-----------+
;;
(define (copy-closure-env-to-stack)
  (let ((copy-loop (new-label "copy_closure"))
        (copy-done (new-label "copy_done")))
    `(;; rax contains closure pointer p
      ;; r8 := p->n
      (mov r8 (offset rax 8))
      ;; r9 := p + 16
      (mov r9 rax)
      (add r9 16)
      (label ,copy-loop)
      (cmp r8 0)
      (je (label ,copy-done))
      ;; rbx := *r9
      (mov rbx (offset r9 0))
      ;; *rcx := rbx
      (mov (offset rcx 0) rbx)
      (sub r8 1)
      (add r9 8)
      (sub rcx 8)
      (jmp (label ,copy-loop))
      (label ,copy-done))))

(define (compile-args args env)
  (if (null? args) '()
    (let ((c0 (compile/1 (first args) env))
          (cs (compile-args (rest args) (cons #f env)))
          (i (- 0 (* (add1 (length env)) 8))))
      `(,@c0
         (mov (offset rsp ,i) rax)
         ,@cs))))

;; Move n arguments up the stack by off slots
(define (move-args n off)
  (if (= n 0) '()
    `(,@(move-args (sub1 n) off)
       ;; Load from address [rsp - (n + off) * 8]
       (mov rbx (offset rsp ,(- 0 (* (+ n off) 8))))
       ;; Write to address [rsp - (n + off - off) * 8] = [rsp - n * 8]
       (mov (offset rsp ,(- 0 (* n 8))) rbx))))

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
    (jne  (label err))))

(define assert-integer
  (assert-type (sub1 32) type-imm))

(define assert-box
  (assert-type (sub1 8) type-box))

(define assert-pair
  (assert-type (sub1 8) type-pair))

(define assert-fun
  (assert-type (sub1 8) type-fun))

;; Prefix label with an underscore (macOS)
(define (label->string label)
  (string-append "_" label))

(define (offset->string base off)
  (if (member? base '(rax rbx rcx rdi rbp rsp r8 r9))
    ;; [Register + Integer]
    (string-append "[" base " + " off "]")
    ;; [Label + Integer]
    (string-append "[rel " (label->string base) " + " off "]")))

(define (operand->string op)
  (cond
    (((unary? 'label) op) (label->string (second op)))
    (((binary? 'offset) op) (offset->string (second op) (third op)))
    (else op)))

(define (operands->string ops)
  (string-append
    (operand->string (first ops)) ", " (operand->string (second ops))))

(define (instr->string instr)
  (let ((opcode   (car instr))
        (operands (cdr instr)))
    (switch opcode
            (('mov  (string-append tab "mov "  (operands->string operands)))
             ('lea  (string-append tab "lea "  (operands->string operands)))
             ('add  (string-append tab "add "  (operands->string operands)))
             ('sub  (string-append tab "sub "  (operands->string operands)))
             ('cmp  (string-append tab "cmp "  (operands->string operands)))
             ('_and (string-append tab "and "  (operands->string operands)))
             ('_or  (string-append tab "or "   (operands->string operands)))
             ('xor  (string-append tab "xor "  (operands->string operands)))
             ('jmp  (string-append tab "jmp "  (operand->string (first operands))))
             ('je   (string-append tab "je "   (operand->string (first operands))))
             ('jne  (string-append tab "jne "  (operand->string (first operands))))
             ('call (string-append tab "call " (operand->string (first operands))))
             ('push (string-append tab "push " (operand->string (first operands))))
             ('ret  (string-append tab "ret"))
             (default ;; Label
               (string-append (label->string (first operands)) ":"))))))

(define (emit instrs)
  (let ((entry (cadar instrs)))
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
