(load "tora/prelude.scm")
(load "../common.scm")

(define (expr? expr)
  (cond
    ((number? expr) #t)
    (((unary? 'add1) expr) (expr? (second expr)))
    (((unary? 'sub1) expr) (expr? (second expr)))
    ;; (if (zero? e0) e1 e2)
    (((ternary? 'if) expr)
     (let ((test-e (second expr))
           (e1 (third expr))
           (e2 (fourth expr)))
       (and
         (and ((unary? 'zero?) test-e) (expr? (second test-e)))
         (and (expr? e1) (expr? e2)))))
    (else #f)))

(define new-label (gensym 0))

(define (compile expr)
  `((entry)
    ,@(compile/1 expr)
    (ret)))

(define (compile/1 expr)
  (cond
    ((number? expr) `((mov rax ,expr)))
    (((unary? 'add1) expr) `(,@(compile/1 (second expr)) (add rax 1)))
    (((unary? 'sub1) expr) `(,@(compile/1 (second expr)) (sub rax 1)))
    (((ternary? 'if) expr)
     (let ((c0 (compile/1 (second (second expr))))
           (c1 (compile/1 (third expr)))
           (c2 (compile/1 (fourth expr)))
           (l0 (new-label "if"))
           (l1 (new-label "if")))
       `(,@c0
          (cmp rax 0)
          (jne ,l0)
          ,@c1
          (jmp ,l1)
          (,l0)
          ,@c2
          (,l1))))
    (else (error "compile/1"))))

;; Prefix label with an underscore (macOS)
(define (label->string label)
  (string-append "_" label))

(define (instr->string instr)
  (let ((opcode   (car instr))
        (operands (cdr instr)))
    (switch opcode
            (('mov (string-append tab "mov " (first operands) ", " (second operands)))
             ('add (string-append tab "add " (first operands) ", " (second operands)))
             ('sub (string-append tab "sub " (first operands) ", " (second operands)))
             ('cmp (string-append tab "cmp " (first operands) ", " (second operands)))
             ('jmp (string-append tab "jmp " (label->string (first operands))))
             ('jne (string-append tab "jne " (label->string (first operands))))
             ('ret (string-append tab "ret"))
             (default ;; Label
               (string-append (label->string opcode) ":"))))))

(define (emit instrs)
  (let ((entry (caar instrs)))
    (begin
      (print (string-append tab "global " (label->string entry)))
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
