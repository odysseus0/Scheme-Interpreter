; ---------
; Datatypes
; ---------

; Define the predicates that check whether a datum is a primitive type (or atomic)

(define (lit? datum)
	(not (pair? datum)))

(define (form? datum)
	(and (pair? datum)
			 (eqv? (car datum) 'quote)))

(define (2-pair? lst)
	(equal? (length lst) 2))

(define (clause? datum)
  (and (or (expression? (1st datum))
           (eqv? (1st datum) 'else))
       ((list-of expression?) (2nd datum))))

(define improper-list-of
  (lambda (pred)
    (lambda (imp-lst)
      (andmap pred (improper-list->proper imp-lst)))))

(define list-implst-of-symbol?
  (lambda (x)
    (or ((list-of symbol?) x)
        ((improper-list-of symbol?) x))))

; > (valid-param? '(ref x))
; #t
; > (valid-param? 'a)
; #t
(define valid-param?
  (lambda (datum)
    (or (symbol? datum)
        (ref? datum))))

(define valid-params?
  (lambda (x)
    (or (symbol? x)
        ((list-of valid-param?) x)
        ((improper-list-of valid-param?) x))))

; Parsed expression datatypes

(define-datatype expression expression?
	[lit-exp (id lit?)]
	[form-exp (form form?)]
	[var-exp (id symbol?)]
	[lambda-exp (formals valid-params?) (bodies (list-of expression?))]
	;[lambda-exp-variable (formals (list-of symbol?)) (bodies (list-of expression?))]
  ;[lambda-exp-improper (formals (list-of symbol?)) (bodies (list-of expression?))]
	[if-then-exp (pred expression?) (then-exp expression?)]
	[if-then-else-exp (pred expression?) (then-exp expression?) (else-exp expression?)]
	[let-exp (vars (list-of symbol?))
					 (exps (list-of expression?))
					 (bodies (list-of expression?))]
	[let*-exp (vars (list-of symbol?))
						(exps (list-of expression?))
						(bodies (list-of expression?))]
	[set-exp (var symbol?) (body expression?)]
  [and-exp (exps (list-of expression?))]
  [or-exp (exps (list-of expression?))]
  [cond-exp (clauses (list-of clause?))]
  [case-exp (expr expression?) (clauses (list-of clause?))]
  [begin-exp (bodies (list-of expression?))]
  [while-exp (test expression?) (bodies (list-of expression?))]
	[app-exp (rator expression?) (rand (list-of expression?))]
  [do1-exp (exps (list-of expression?)) (test-exp expression?)]
  [do2-exp (exps (list-of expression?)) (test-exp expression?)]
  [call-with-values-exp (producer expression?) (consumer expression?)]
  [letrec-exp (proc-names (list-of symbol?)) (idss (list-of list-implst-of-symbol?))
              (bodiess (list-of (list-of expression?))) (letrec-bodies (list-of expression?))]
  [named-let-exp (name symbol?)
                 (vars (list-of symbol?))
                 (exps (list-of expression?))
                 (bodies (list-of expression?))]
  [define-exp (var symbol?) (exp expression?)])

; Environment type definitions

(define scheme-value?
	(lambda (x) #t))

(define-datatype environment environment?
	(empty-env-record)
	(extended-env-record
    (syms (list-of symbol?))
    (vals vector?)
    (env environment?))
  [recursively-extended-env-record
   (proc-names (list-of symbol?))
   (idss (list-of list-implst-of-symbol?))
   (bodiess (list-of (list-of expression?)))
   (env environment?)])

; Datatype for procedures.

(define-datatype proc-val proc-val?
	[prim-proc (name symbol?)]
	[closure (params list-implst-symbol?)
           (ref-params (list-of symbol?))
					 (bodies (list-of expression?))
					 (env environment?)])
  ;[closure-lambda-var (param (list-of symbol?))
  ;                    (bodies (list-of expression?))
  ;                    (env environment?)]
  ;[closure-lambda-improper (params (list-of symbol?))
  ;                         (bodies (list-of expression?))
  ;                         (env environment?)])

(define-datatype reference reference?
  [refer (vals vector?) (index number?)])

(define deref
  (lambda (ref)
    (cases reference ref
           (refer (vals index)
                  (vector-ref vals index)))))

(define set-ref!
  (lambda (ref val)
    (cases reference ref
           (refer (vals index)
                  (vector-set! vals index val)))))

(define cell
  (lambda (value)
    (box value)))

(define cell?
  (lambda (obj)
    (box? obj)))

(define cell-ref
  (lambda (cell)
    (unbox cell)))

(define cell-set!
  (lambda (cell value)
    (set-box! cell value)))
