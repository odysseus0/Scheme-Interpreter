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

(define list-implst-symbol?
  (lambda (x)
    (or (symbol? x)
        ((list-of symbol?) x)
        ((improper-list-of symbol?) x))))


; Parsed expression datatypes

(define-datatype expression expression?
	[lit-exp (id lit?)]
	[form-exp (form form?)]
	[var-exp (id symbol?)]
	[lambda-exp (formals list-implst-symbol?) (bodies (list-of expression?))]
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
	[app-exp (rator expression?) (rand (list-of expression?))]
  [letrec-exp (proc-names (list-of symbol?)) (idss (list-of list-implst-of-symbol?))
              (bodiess (list-of (list-of expression?))) (letrec-bodies (list-of expression?))]
  [named-let-exp (name symbol?)
                 (vars (list-of symbol?))
                 (exps (list-of expression?))
                 (bodies (list-of expression?))]
  [define-exp (var symbol?) (exp expression?)])


;;; Continuation type definitions
(define-datatype continuation continuation?
  [init-k]
  [rator-k (rands (list-of expression?)) (env environment?) (k continuation?)]
  [rands-k (proc-value proc-val) (k continuation)]
  [test-k (then-exp expression?) (else-exp expression?)
          (env environment?) (k continuation?)]
  [test-k2 (then-exp expression?) (env environment?) (k continuation?)]
  [extend-env-k (bodies (list-of expression?)) (k continuation?)]
  [define-k (init-env environment?) (var symbol?) (k continuation?)]
  [define-extend-k (init-env environment?) (k continuation?)]
  [set-body-k (env environment?) (var symbol?) (k continuation?)]
  [eval-bodies-k (bodies (list-of expression?)) (env environment?) (k continuation?)])


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
					 (bodies (list-of expression?))
					 (env environment?)])

(define-datatype reference reference?
  [refer (vals vector?) (index number?)])

(define deref
  (lambda (ref)
    (cases reference ref
           (refer (vals index)
                  (vector-ref vals index)))))

(define set-ref!
  (lambda (ref val k)
    (cases reference ref
           (refer (vals index)
                  (apply-k k (vector-set! vals index val))))))
