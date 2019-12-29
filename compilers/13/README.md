[Jig: jumping to tail calls](http://www.cs.umd.edu/class/fall2019/cmsc430/Jig.html)

```diff
--- ../12/test.s
+++ test.s
@@ -3,10 +3,10 @@
 	section .text
 _entry:
 	mov rax, 3232
-	mov [rsp + -16], rax
-	sub rsp, 0
-	call _even?
-	add rsp, 0
+	mov [rsp + -8], rax
+	mov rbx, [rsp + -8]
+	mov [rsp + -8], rbx
+	jmp _even?
 	ret
 _err:
 	push rbp
@@ -35,10 +35,10 @@
 	cmp rbx, 0
 	jne _err
 	sub rax, 32
-	mov [rsp + -24], rax
-	sub rsp, 8
-	call _odd?
-	add rsp, 8
+	mov [rsp + -16], rax
+	mov rbx, [rsp + -16]
+	mov [rsp + -8], rbx
+	jmp _odd?
 _L_if_007:
 	ret
 _odd?:
@@ -65,9 +65,9 @@
 	cmp rbx, 0
 	jne _err
 	sub rax, 32
-	mov [rsp + -24], rax
-	sub rsp, 8
-	call _even?
-	add rsp, 8
+	mov [rsp + -16], rax
+	mov rbx, [rsp + -16]
+	mov [rsp + -8], rbx
+	jmp _even?
 _L_if_003:
 	ret
```
