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

; Parsed expression datatypes

(define-datatype expression expression?
	[lit-exp (id lit?)]
	[form-exp (form form?)]
	[var-exp (id symbol?)]
	[lambda-exp (formals (list-of symbol?)) (body (list-of expression?))]
	[lambda-exp-variable (formal symbol?) (body (list-of expression?))]
	[if-then-exp (pred expression?) (then-exp expression?)]
	[if-then-else-exp (pred expression?) (then-exp expression?) (else-exp expression?)]
	[let-exp (vars (list-of symbol?))
					 (exps (list-of expression?))
					 (bodys (list-of expression?))]
	[let*-exp (vars (list-of symbol?))
						(exps (list-of expression?))
						(bodys (list-of expression?))]
	[letrec-exp (vars (list-of symbol?))
							(exps (list-of expression?))
							(bodys (list-of expression?))]
	[set-exp (var symbol?) (body expression?)]
	[app-exp (rator expression?) (rand (list-of expression?))])

; Environment type definitions

(define scheme-value?
	(lambda (x) #t))

(define-datatype environment environment?
	(empty-env-record)
	(extended-env-record
    (syms (list-of symbol?))
    (vals (list-of scheme-value?))
    (env environment?)))

; Datatype for procedures.

(define-datatype proc-val proc-val?
	[prim-proc (name symbol?)]
	[closure (params (list-of symbol?))
					 (bodies (list-of expression?))
					 (env environment?)])

