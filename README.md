tora-scheme
===========

A tiny Lisp/Scheme plaything

Usage
-----

The interpreter is written in Lua. With Lua installed, you can start an
interactive REPL

```
./tora.lua
λ> (print "Hello World!")
"Hello World!"
```

or interpret the contents of a file

```
./tora.lua example.scm
```

Loading a file with `-l` brings the definitions into scope:

```
./tora.lua -l tora/prelude.scm
λ> (sum '(1 2 3))
6
```

Language Features
-----------------

So far tora has:
- Simple data types (mapped to host language): numbers, booleans,
  strings, symbols, lists
- Special forms: `if`, `cond`, `begin`, `define`, `let`, `letrec`, `set!`,
  `lambda`, `quote` (`'`), `quasiquote` (`` ` ``), `unquote` (`,`),
  `unquote-splicing` (`,@`)
- Built-in functions and predicates, including `+`, `-`, `*`, `/`, `=`, `<`,
  `<=`, `>`, `>=`, `list`, `cons`, `car`, `cdr`, `number?`, `boolean?`,
  `string?`, `symbol?`, `null?`, `pair?`, `list?`, `equal?`
- A small collection of functions written in Scheme, including `map`,
  `filter`, and `fold`

Enter the REPL and hit `:b` for a list of all defined symbols in the global
environment.

TODO
----

- [x] `let` bindings
- [x] Quasiquotation
- [ ] Basic macros
- [ ] Error handling
- [ ] String functions
- [ ] N-ary operators
- [ ] Nicer REPL
- [ ] ...

References
----------

- [CSE 341 -- Scheme Basics](http://courses.cs.washington.edu/courses/cse341/03wi/scheme/basics.html)
- [(How to Write a (Lisp) Interpreter (in Python))](http://norvig.com/lispy.html)
- [Lisp as the Maxwell's equations of software](http://www.michaelnielsen.org/ddi/lisp-as-the-maxwells-equations-of-software)
- [Quasiquotation in Lisp](http://repository.readscheme.org/ftp/papers/pepm99/bawden.pdf)
- [Make a Lisp](https://github.com/kanaka/mal/blob/master/process/guide.md)
