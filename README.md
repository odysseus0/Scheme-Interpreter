# Scheme Interpreter

A metacircular Scheme interpreter — Scheme code that interprets Scheme. Built as a CS304 (Programming Languages) project exploring language implementation concepts.

## Features

**Core Language**
- First-class procedures with lexical closures
- Tail-call optimization via continuation-passing style (CPS)
- `call/cc` for first-class continuations
- Mutable state (`set!`, `set-car!`, `set-cdr!`)
- Variable-arity procedures (rest arguments)

**Special Forms**
- `lambda`, `if`, `define`, `set!`
- `let`, `let*`, `letrec`, named `let`
- `and`, `or`, `cond`, `case`, `begin`
- `quasiquote` with `unquote` and `unquote-splicing`

**Syntax Expansion**
Derived forms (`let`, `letrec`, `cond`, etc.) are expanded to core forms before evaluation — the classic technique from SICP.

**60+ Primitives**
Arithmetic, list operations, vectors, predicates, `map`, `apply`, and more.

## Usage

Requires [Chez Scheme](https://cisco.github.io/ChezScheme/) (tested on v10.3.0).

```bash
# Install Chez Scheme (macOS)
brew install chezscheme

# Run the interpreter
cd Scheme-Interpreter
chez --script main.ss  # or: chez, then (load "main.ss")
```

### REPL

```scheme
(rep)  ; Start the REPL

--> (define fact (lambda (n) (if (= n 0) 1 (* n (fact (- n 1))))))
--> (fact 10)
3628800

--> (call/cc (lambda (k) (+ 1 (k 42))))
42
```

### Single Expression

```scheme
(eval-one-exp '(let ((x 10)) (* x x)))
; => 100

(eval-one-exp '((lambda (f) (f f 5))
                (lambda (self n)
                  (if (= n 0) 1 (* n (self self (- n 1)))))))
; => 120
```

### Examples

```scheme
(load "examples.ss")
(run-all-examples)
```

Includes Y combinator, Church numerals, and `amb` operator (non-deterministic choice via `call/cc`).

## Architecture

```
main.ss             Entry point, loads modules
├── chez-init.ss    Chez Scheme compatibility layer (define-datatype, etc.)
├── datatypes.ss    AST nodes, continuations, closures, environments
├── parser.ss       S-expression → AST
├── environments.ss Lexical environments with mutable bindings
├── interpreter.ss  CPS evaluator + syntax expansion + primitives
├── examples.ss     Y combinator, Church numerals, amb operator
└── tests.ss        Test suite
```

## Authors

- Tengji Zhang
- Shunfan Du
