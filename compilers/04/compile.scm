(load "src/prelude.scm")
(load "../common.scm")

(define unary-primitive? (unary? '(add1 sub1)))

(define (expr? expr)
  (cond
    ((number? expr) #t)
    ((unary-primitive? expr) (expr? (second expr)))
    (else #f)))

(define (compile expr)
  `((entry)
    ,@(compile/1 expr)
    (ret)))

(define (compile/1 expr)
  (cond
    ((number? expr) `((mov rax ,expr)))
    (((unary? 'add1) expr) `(,@(compile/1 (second expr)) (add rax 1)))
    (((unary? 'sub1) expr) `(,@(compile/1 (second expr)) (sub rax 1)))
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
