(load "tora/prelude.scm")
(load "../common.scm")

(define unary-primitives
  '(add1 sub1 zero? box unbox car cdr))

(define binary-primitives
  '(+ - cons))

(define all-primitives
  `(,@unary-primitives ,@binary-primitives if let begin define))

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

    ;; (begin
    ;;   (define (f0 ...) e0)
    ;;   (define (f1 ...) e1)
    ;;   ...
    ;;   e)
    ((equal? (first expr) 'begin)
     (let ((expr-and-defs (reverse (rest expr))))
       (and (expr? (first expr-and-defs))
            (all expr-define? (rest expr-and-defs)))))

    ;; Function call: (f ...)
    ((variable? (first expr)) (all expr? (rest expr)))

    (else #f)))

;; (define (f0 ...) e0) is desugared to
;; (define f0 (lambda (...) e0))
(define (expr-define? expr)
  (if (not ((binary? 'define) expr)) #f
    (let ((f0 (second expr))
          (xs (cadr (third expr)))
          (e0 (caddr (third expr))))
      (and (variable? f0) (and (all variable? xs) (expr? e0))))))

(define new-label (gensym 0))

(define (compile expr)
  (cond
    ;; (begin
    ;;   (define (f0 ...) e0)
    ;;   (define (f1 ...) e1)
    ;;   ...
    ;;   e)
    ((and (list? expr) (equal? (first expr) 'begin))
     (let ((expr-and-defs (reverse (rest expr))))
       (let ((ds (compile-defines (rest expr-and-defs)))
             (c0 (compile-entry (first expr-and-defs))))
         `(,@c0
            ,@ds))))
    ;; Single expression
    (else
      (compile-entry expr))))

(define (compile-entry expr)
  `((entry)
    ,@(compile-tail/1 expr '())
    (ret)
    (err)
    (push rbp)
    (call error)))

;; Compile expr in non-tail position
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

    ;; Function call: (f ...)
    ((variable? (first expr))
     (let ((f (first expr))
           (xs (rest expr)))
       (compile-call f xs env)))

    (else (error "compile/1"))))

;; Compile expr in tail position
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

    ;; Function call: (f ...)
    ((variable? (first expr))
     (let ((f (first expr))
           (xs (rest expr)))
       (compile-tail-call f xs env)))

    (else (error "compile-tail/1"))))

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
       (jne ,l0)
       (mov rax 8)
       (jmp ,l1)
       (,l0)
       (mov rax 16)
       (,l1))))

(define type-imm  0) ;; 0b000
(define type-box  1) ;; 0b001
(define type-pair 2) ;; 0b010

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
        (c0 (compile/1 e0 (cons #f env))))
    (if (member? op '(+ -))
      ;; (+ e0 e1) | (- e0 e1)
      `(,@c1
         ,@assert-integer
         (mov (offset rsp ,(- 0 (* (add1 (length env)) 8))) rax)
         ,@c0
         ,@assert-integer
         (,(switch op (('+ 'add) ('- 'sub)))
           rax (offset rsp ,(- 0 (* (add1 (length env)) 8)))))
      ;; (cons e0 e1)
      `(,@c1
         (mov (offset rsp ,(- 0 (* (add1 (length env)) 8))) rax)
         ,@c0
         (mov (offset rdi 0) rax)
         (mov rax (offset rsp ,(- 0 (* (add1 (length env)) 8))))
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
       (jne ,l0)
       ,@c1
       (jmp ,l1)
       (,l0)
       ,@c2
       (,l1))))

(define (compile-tail-if e0 e1 e2 env)
  (let ((c0 (compile/1 e0 env))
        (c1 (compile-tail/1 e1 env))
        (c2 (compile-tail/1 e2 env))
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

(define (compile-tail-let x e0 e1 env)
  (let ((c0 (compile/1 e0 env))
        (c1 (compile-tail/1 e1 (cons x env))))
    `(,@c0
       (mov (offset rsp ,(- 0 (* (add1 (length env)) 8))) rax)
       ,@c1)))

(define (compile-defines lst)
  (foldl append '() (map compile-define lst)))

(define (compile-define expr)
  (let ((f (second expr))
        (xs (cadr (third expr)))
        (e0 (caddr (third expr))))
    (let ((c0 (compile-tail/1 e0 (reverse xs))))
      `((,f) ;; TODO: (symbol->label f)
        ,@c0
        (ret)))))

;; Compile a function call in non-tail position
(define (compile-call f xs env)
  (let ((cs (compile-args xs (cons #f env)))
        (sz (* (length env) 8)))
    `(,@cs
       (sub rsp ,sz)
       (call ,f)
       (add rsp ,sz))))

;; Compile a function call in tail position
(define (compile-tail-call f xs env)
  (let ((cs (compile-args xs env)))
    `(,@cs
       ;; Reuse stack space
       ,@(move-args (length xs) (length env))
       (jmp ,f))))

;; Move n arguments up the stack by off slots
(define (move-args n off)
  (if (= n 0) '()
    `(,@(move-args (sub1 n) off)
       ;; Load from address [rsp - (n + off) * 8]
       (mov rbx (offset rsp ,(- 0 (* (+ n off) 8))))
       ;; Write to address [rsp - (n + off - off) * 8] = [rsp - n * 8]
       (mov (offset rsp ,(- 0 (* n 8))) rbx))))

(define (compile-args args env)
  (if (null? args) '()
    (let ((c0 (compile/1 (first args) env))
          (cs (compile-args (rest args) (cons #f env))))
      `(,@c0
         (mov (offset rsp ,(- 0 (* (add1 (length env)) 8))) rax)
         ,@cs))))

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

(define assert-integer
  (assert-type (sub1 32) type-imm))

(define assert-box
  (assert-type (sub1 8) type-box))

(define assert-pair
  (assert-type (sub1 8) type-pair))

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
