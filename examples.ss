;;; examples.ss - Showcase demonstrations of the Scheme interpreter
;;; These examples demonstrate CS fundamentals using features the interpreter supports.
;;;
;;; Load: (load "examples.ss")
;;; Run:  (run-all-examples)

;;; =============================================================================
;;; Y COMBINATOR - Recursion without define
;;; =============================================================================
;;; The Y combinator enables recursive functions without explicit self-reference.
;;; This is the call-by-value version (Z combinator), required for strict evaluation.

(define Y
  (lambda (f)
    ((lambda (x) (f (lambda (v) ((x x) v))))
     (lambda (x) (f (lambda (v) ((x x) v)))))))

;;; Factorial using Y - no self-reference in the function body
(define fact-y
  (Y (lambda (fact)
       (lambda (n)
         (if (= n 0)
             1
             (* n (fact (- n 1))))))))

;;; Fibonacci using Y
(define fib-y
  (Y (lambda (fib)
       (lambda (n)
         (cond ((= n 0) 0)
               ((= n 1) 1)
               (else (+ (fib (- n 1)) (fib (- n 2)))))))))

;;; =============================================================================
;;; CHURCH NUMERALS - Numbers as pure lambda functions
;;; =============================================================================
;;; Church encoding represents natural numbers using only lambda calculus.
;;; A Church numeral n is a function that applies f to x exactly n times.

(define church-zero (lambda (f) (lambda (x) x)))
(define church-succ (lambda (n) (lambda (f) (lambda (x) (f ((n f) x))))))

(define church-one   (church-succ church-zero))
(define church-two   (church-succ church-one))
(define church-three (church-succ church-two))
(define church-four  (church-succ church-three))
(define church-five  (church-succ church-four))

;;; Church arithmetic - addition is function composition
(define church-add
  (lambda (m)
    (lambda (n)
      (lambda (f)
        (lambda (x)
          ((m f) ((n f) x)))))))

;;; Multiplication: apply m, n times
(define church-mult
  (lambda (m)
    (lambda (n)
      (lambda (f)
        (m (n f))))))

;;; Exponentiation: m^n
(define church-exp
  (lambda (m)
    (lambda (n)
      (n m))))

;;; Convert Church numeral to Scheme integer
(define church->int
  (lambda (cn)
    ((cn (lambda (x) (+ x 1))) 0)))

;;; Convert Scheme integer to Church numeral
(define int->church
  (lambda (n)
    (if (= n 0)
        church-zero
        (church-succ (int->church (- n 1))))))

;;; =============================================================================
;;; AMB OPERATOR - Non-deterministic choice with backtracking
;;; =============================================================================
;;; The amb operator implements backtracking search using call/cc.
;;; When (require #f) is called, execution backtracks to try the next choice.

(define *fail-stack* '())

(define fail
  (lambda ()
    (if (null? *fail-stack*)
        'no-more-solutions
        (let ((back-track-point (car *fail-stack*)))
          (set! *fail-stack* (cdr *fail-stack*))
          (back-track-point 'fail)))))

(define amb
  (lambda choices
    (call/cc
     (lambda (return)
       (let loop ((choices choices))
         (if (null? choices)
             (fail)
             (begin
               (call/cc
                (lambda (try-next)
                  (set! *fail-stack* (cons try-next *fail-stack*))
                  (return (car choices))))
               (loop (cdr choices)))))))))

(define require
  (lambda (pred)
    (if (not pred) (fail))))

(define amb-reset
  (lambda ()
    (set! *fail-stack* '())))

;;; Find a Pythagorean triple using amb
(define pythagorean-triple
  (lambda ()
    (amb-reset)
    (let ((a (amb 1 2 3 4 5 6 7 8 9 10))
          (b (amb 1 2 3 4 5 6 7 8 9 10))
          (c (amb 1 2 3 4 5 6 7 8 9 10)))
      (require (<= a b))
      (require (<= b c))
      (require (= (+ (* a a) (* b b)) (* c c)))
      (list a b c))))

;;; =============================================================================
;;; TAIL CALL OPTIMIZATION VERIFICATION
;;; =============================================================================
;;; These functions would overflow the stack without proper TCO.
;;; The interpreter uses CPS internally, ensuring tail calls don't grow the stack.

(define countdown
  (lambda (n)
    (if (= n 0)
        'done
        (countdown (- n 1)))))

(define sum-tail
  (lambda (n acc)
    (if (= n 0)
        acc
        (sum-tail (- n 1) (+ n acc)))))

;;; Mutual recursion - also requires TCO
(define even-tco?
  (lambda (n)
    (if (= n 0) #t (odd-tco? (- n 1)))))

(define odd-tco?
  (lambda (n)
    (if (= n 0) #f (even-tco? (- n 1)))))

;;; =============================================================================
;;; RUN ALL EXAMPLES
;;; =============================================================================

(define run-all-examples
  (lambda ()
    (display "=== Y Combinator ===") (newline)
    (display "Factorial of 10 (using Y): ")
    (display (fact-y 10)) (newline)
    (display "Fibonacci of 10 (using Y): ")
    (display (fib-y 10)) (newline)
    (newline)

    (display "=== Church Numerals ===") (newline)
    (display "2 + 3 = ")
    (display (church->int ((church-add church-two) church-three))) (newline)
    (display "2 * 3 = ")
    (display (church->int ((church-mult church-two) church-three))) (newline)
    (display "2 ^ 3 = ")
    (display (church->int ((church-exp church-two) church-three))) (newline)
    (newline)

    (display "=== Amb Operator (Backtracking Search) ===") (newline)
    (display "Pythagorean triple (using call/cc backtracking): ")
    (display (pythagorean-triple)) (newline)
    (newline)

    (display "=== Tail Call Optimization ===") (newline)
    (display "Countdown from 100000: ")
    (display (countdown 100000)) (newline)
    (display "Sum 1 to 10000: ")
    (display (sum-tail 10000 0)) (newline)
    (display "Is 10000 even? (mutual recursion): ")
    (display (even-tco? 10000)) (newline)
    (newline)

    'examples-complete))
