;;; tests.ss - Test suite for the Scheme interpreter
;;; Run: (load "tests.ss") then (run-tests)

(define test-count 0)
(define pass-count 0)
(define fail-count 0)

(define test
  (lambda (name expected expr)
    (set! test-count (+ test-count 1))
    (let ((result (eval-one-exp expr)))
      (if (equal? result expected)
          (begin
            (set! pass-count (+ pass-count 1))
            (display "."))
          (begin
            (set! fail-count (+ fail-count 1))
            (newline)
            (display "FAIL: ")
            (display name)
            (newline)
            (display "  Expected: ")
            (display expected)
            (newline)
            (display "  Got: ")
            (display result)
            (newline))))))

(define run-tests
  (lambda ()
    (set! test-count 0)
    (set! pass-count 0)
    (set! fail-count 0)
    (reset-global-env)

    (display "Running tests")
    (newline)

    ;;; Basic Arithmetic
    (test "addition" 5 '(+ 2 3))
    (test "subtraction" 7 '(- 10 3))
    (test "multiplication" 24 '(* 4 6))
    (test "division" 5 '(/ 15 3))
    (test "nested arithmetic" 14 '(+ (* 2 3) (- 10 2)))
    (test "add1" 6 '(add1 5))
    (test "sub1" 4 '(sub1 5))
    (test "zero?" #t '(zero? 0))
    (test "zero? false" #f '(zero? 5))
    (test "comparisons" #t '(< 1 2))
    (test "equality" #t '(= 5 5))

    ;;; Lambda and Closures
    (test "simple lambda" 10 '((lambda (x) (* x 2)) 5))
    (test "multi-arg lambda" 11 '((lambda (x y) (+ x y)) 5 6))
    (test "closure" 15 '(let ((x 10)) ((lambda (y) (+ x y)) 5)))
    (test "nested closure" 30
          '(let ((x 10))
             (let ((f (lambda (y) (+ x y))))
               (f 20))))
    (test "currying" 6 '(((lambda (x) (lambda (y) (* x y))) 2) 3))
    (test "rest args" '(2 3 4) '((lambda args args) 2 3 4))
    (test "rest with fixed" '(1 (2 3)) '((lambda (a . b) (list a b)) 1 2 3))
    (test "no args lambda" 42 '((lambda () 42)))

    ;;; Let Forms
    (test "let" 30 '(let ((x 5) (y 6)) (* x y)))
    (test "let sequential body" 11 '(let ((x 5)) (+ x 1) (+ x 6)))
    (test "let*" 10 '(let* ((x 5) (y x)) (+ x y)))
    (test "let* shadowing" 20 '(let* ((x 5) (x (* x 2)) (x (* x 2))) x))
    (test "letrec factorial" 120
          '(letrec ((fact (lambda (n)
                            (if (= n 0) 1 (* n (fact (- n 1)))))))
             (fact 5)))
    (test "letrec mutual" #t
          '(letrec ((even? (lambda (n) (if (= n 0) #t (odd? (- n 1)))))
                    (odd? (lambda (n) (if (= n 0) #f (even? (- n 1))))))
             (even? 10)))
    (test "named let" 55
          '(let loop ((i 10) (acc 0))
             (if (= i 0) acc (loop (- i 1) (+ acc i)))))

    ;;; Conditionals
    (test "if true" 1 '(if #t 1 2))
    (test "if false" 2 '(if #f 1 2))
    (test "if no else" 5 '(if #t 5))
    (test "cond first" 'one '(cond ((= 1 1) 'one) (else 'other)))
    (test "cond second" 'two '(cond ((= 1 2) 'one) ((= 2 2) 'two) (else 'other)))
    (test "cond else" 'other '(cond ((= 1 2) 'one) (else 'other)))
    (test "case" 'one '(case 1 ((1) 'one) ((2) 'two) (else 'other)))
    (test "and true" 3 '(and 1 2 3))
    (test "and false" #f '(and 1 #f 3))
    (test "and empty" #t '(and))
    (test "or first true" 1 '(or 1 2 3))
    (test "or skip false" 2 '(or #f 2 3))
    (test "or all false" #f '(or #f #f))
    (test "or empty" #f '(or))

    ;;; call/cc
    (test "call/cc return" 42 '(call/cc (lambda (k) (k 42))))
    (test "call/cc escape" 10 '(+ 5 (call/cc (lambda (k) (k 5)))))
    (test "call/cc no escape" 10 '(call/cc (lambda (k) (+ 3 7))))
    (test "call/cc early exit" 3
          '(call/cc (lambda (return)
                      (let loop ((i 0))
                        (if (= i 3)
                            (return i)
                            (loop (+ i 1)))))))

    ;;; set! and Mutation
    (test "set!" 20
          '(let ((x 10))
             (set! x 20)
             x))
    (test "set! closure" 1
          '(let ((counter 0))
             (let ((inc (lambda () (set! counter (+ counter 1)) counter)))
               (inc))))
    (test "set-car!" '(99 2 3)
          '(let ((p (list 1 2 3)))
             (set-car! p 99)
             p))
    (test "set-cdr!" '(1 . 99)
          '(let ((p (cons 1 2)))
             (set-cdr! p 99)
             p))

    ;;; List Operations
    (test "cons" '(1 . 2) '(cons 1 2))
    (test "cons list" '(1 2 3) '(cons 1 (cons 2 (cons 3 '()))))
    (test "car" 1 '(car '(1 2 3)))
    (test "cdr" '(2 3) '(cdr '(1 2 3)))
    (test "cadr" 2 '(cadr '(1 2 3)))
    (test "list" '(1 2 3) '(list 1 2 3))
    (test "null?" #t '(null? '()))
    (test "null? false" #f '(null? '(1)))
    (test "length" 4 '(length '(a b c d)))
    (test "append" '(1 2 3 4) '(append '(1 2) '(3 4)))
    (test "assq found" '(b 2) '(assq 'b '((a 1) (b 2) (c 3))))
    (test "assq not found" #f '(assq 'd '((a 1) (b 2) (c 3))))
    (test "memv found" '(2 3) '(memv 2 '(1 2 3)))

    ;;; Predicates
    (test "pair?" #t '(pair? '(1 . 2)))
    (test "pair? list" #t '(pair? '(1 2)))
    (test "pair? empty" #f '(pair? '()))
    (test "list?" #t '(list? '(1 2 3)))
    (test "list? improper" #f '(list? '(1 . 2)))
    (test "number?" #t '(number? 42))
    (test "symbol?" #t '(symbol? 'foo))
    (test "procedure?" #t '(procedure? (lambda (x) x)))
    (test "atom?" #t '(atom? 5))
    (test "atom? symbol" #t '(atom? 'x))
    (test "eq?" #t '(eq? 'a 'a))
    (test "equal?" #t '(equal? '(1 2) '(1 2)))
    (test "not" #t '(not #f))

    ;;; Higher-order
    (test "map" '(2 4 6) '(map (lambda (x) (* x 2)) '(1 2 3)))
    (test "apply" 6 '(apply + '(1 2 3)))
    (test "apply with args" 10 '(apply + '(1 2 3 4)))

    ;;; Vectors
    (test "vector" '#(1 2 3) '(vector 1 2 3))
    (test "vector-ref" 2 '(vector-ref '#(1 2 3) 1))
    (test "make-vector" '#(0 0 0) '(make-vector 3 0))
    (test "vector?" #t '(vector? '#(1 2)))
    (test "list->vector" '#(a b c) '(list->vector '(a b c)))
    (test "vector->list" '(1 2 3) '(vector->list '#(1 2 3)))

    ;;; Begin and Sequencing
    (test "begin" 3 '(begin 1 2 3))
    (test "begin side effect" 10
          '(let ((x 0))
             (begin (set! x 5) (set! x (* x 2)) x)))

    ;;; Quote
    (test "quote symbol" 'hello '(quote hello))
    (test "quote list" '(1 2 3) ''(1 2 3))

    ;;; Quasiquote
    (test "quasiquote basic" '(a 5 b) '(let ((x 5)) `(a ,x b)))
    (test "quasiquote expr" '(sum 10) '(let ((a 3) (b 7)) `(sum ,(+ a b))))
    (test "quasiquote splice" '(1 2 3 4 5) '(let ((mid '(2 3 4))) `(1 ,@mid 5)))
    (test "quasiquote nested list" '((1 2) (3 4)) '`((1 2) (3 4)))
    (test "quasiquote no unquote" '(a b c) '`(a b c))

    (newline)
    (newline)
    (display "Tests: ")
    (display test-count)
    (display ", Passed: ")
    (display pass-count)
    (display ", Failed: ")
    (display fail-count)
    (newline)
    (if (= fail-count 0)
        (display "All tests passed!")
        (display "Some tests failed."))
    (newline)
    (newline)))
