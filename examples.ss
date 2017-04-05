;;; examples.ss - Showcase demonstrations of the Scheme interpreter
;;;
;;; Load: (load "examples.ss")
;;; Run:  (run-all-examples)

;;; =============================================================================
;;; Y COMBINATOR - Recursion without define
;;; =============================================================================
;;; The Y combinator enables recursive functions without explicit self-reference.
;;; It works by passing a function to itself: f(f) where f expects itself as arg.
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

    (display "=== Tail Call Optimization ===") (newline)
    (display "Countdown from 100000: ")
    (display (countdown 100000)) (newline)
    (display "Sum 1 to 10000: ")
    (display (sum-tail 10000 0)) (newline)
    (display "Is 10000 even? (mutual recursion): ")
    (display (even-tco? 10000)) (newline)
    (newline)

    'examples-complete))
