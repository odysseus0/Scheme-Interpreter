; -----------
; Interpreter
; -----------

; The top-level-eval evaluates a form in the global environment

(define top-level-eval
	(lambda (form)
    ; later we may add things that are not expressions.
		(eval-exp form init-env)))

; The eval-exp is the main component of the interpreter
;;; @param exp: expression dataype; env: envirionment datatype
;;; @return scheme-value, prim-proc

(define eval-exp
	(lambda (exp env)
		(cases expression exp
					 [lit-exp (datum) datum]
					 [form-exp (datum) (2nd datum)]
					 [var-exp (id)
										(apply-env env id ; look up its value.
															 (lambda (x) x) ; procedure to call if id is in the environment 
															 (lambda () (eopl:error 'apply-env ; procedure to call if id not in env
																											"variable not found in environment: ~s"
																											id)))]
					 [let-exp (vars exps bodies)
										(let ([extended-env (extend-env vars
																										(eval-rands exps env)
																										env)])
											(eval-bodies bodies extended-env))]
					 [app-exp (rator rands)
										(let ([proc-value (eval-exp rator env)]
													[args (eval-rands rands env)])
											(apply-proc proc-value args))]
					 [if-then-else-exp (test-exp then-exp else-exp)
														 (if (eval-exp test-exp env)
																 (eval-exp then-exp env)
																 (eval-exp else-exp env))]
           [if-then-exp (test-exp then-exp)
                        (if (eval-exp test-exp env)
                            (eval-exp then-exp env))]
					 [lambda-exp (params bodies)
											 (closure params bodies env)]
           [lambda-exp-variable (formals bodies)
                                (closure-lambda-var formals bodies env)]
           [lambda-exp-improper (formals bodies)
                                (closure-lambda-improper formals bodies env)]

					 [else (eopl:error 'eval-exp "Bad abstract syntax: ~a" exp)])))

; Evaluate the list of operands, putting results into a list

(define eval-rands
	(lambda (rands env)
		(map (lambda (x) (eval-exp x env))
				 rands)))

(define eval-bodies
	(lambda (bodies env)
		(let loop ([bodies bodies])
			(if (null? (cdr bodies))
					(eval-exp (car bodies) env)
					(begin
						(eval-exp (car bodies) env)
						(loop (cdr bodies)))))))

;  Apply a procedure to its arguments.

;;; Helper procedure that will allow extend-env bind varialbes in the correct fashion for
;;; closure-lambda-improper
(define (imp-helper formals args)
  (if (null? (cdr formals))
      (list args)
      (cons (car args) (imp-helper (cdr formals) (cdr args)))))

;;; proc-val: proc-value datatype
;;; @return scheme-value
(define apply-proc
	(lambda (proc-value args)
		(cases proc-val proc-value
					 [prim-proc (op) (apply-prim-proc op args)]
					 [closure (params bodies env)
										(let ([extended-env (extend-env params
																										args
																										env)])
											(eval-bodies bodies extended-env))]
           [closure-lambda-var (formals bodies env)
                               (let ([extended-env (extend-env formals
                                                               (list args)
                                                               env)])
                                 (eval-bodies bodies extended-env))]
           [closure-lambda-improper (formals bodies env)
                                    (let ([extended-env (extend-env formals
                                                                   (imp-helper formals args)
                                                                   env)])
                                      (eval-bodies bodies extended-env))]

                                        ; You will add other cases
					 [else (eopl:error 'apply-proc
														 "Attempt to apply bad procedure: ~s"
														 proc-value)])))

(define *prim-proc-names*
	'(+ - * / add1 sub1 zero? not = < > <= >= cons car cdr
		caar cadr cdar cddr caaar caadr cadar cdaar caddr cdadr
		cddar cdddr list null? assq eq? equal? atom? length
		list->vector list? pair? procedure? vector->list
		vector make-vector vector-ref vector? number? symbol?
		set-car! set-cdr! vector-set! display newline procedure? apply map))

(define init-env         ; for now, our initial global environment only contains 
	(extend-env            ; procedure names.  Recall that an environment associates
	 *prim-proc-names*     ; a value (not an expression) with an identifier.
	 (map prim-proc
				*prim-proc-names*)
	 (empty-env)))

; Usually an interpreter must define each 
; built-in procedure individually.  We are "cheating" a little bit.

(define-syntax try
	(syntax-rules ()
		[(_ procName proc)
		 (with-exception-handler
			(lambda (x)
				(eopl:error procName "Incorrect Arguments"))
			(lambda () proc))]))

; prim-proc: symbols, args: scheme-values
(define apply-prim-proc
	(lambda (prim-proc args)
		(case prim-proc
			[(+) (try '+ (apply + args))]
			[(-) (apply - args)]
			[(*) (apply * args)]
			[(/) (/ (1st args) (2nd args))]
			[(not) (not (1st args))]
			[(add1) (+ (1st args) 1)]
			[(sub1) (- (1st args) 1)]
			[(zero?) (equal? 0 (1st args))]
			[(cons) (cons (1st args) (2nd args))]
			[(=) (= (1st args) (2nd args))]
			[(<) (< (1st args) (2nd args))]
			[(>) (> (1st args) (2nd args))]
			[(<=) (<= (1st args) (2nd args))]
			[(>=) (>= (1st args) (2nd args))]
			[(car) (car (1st  args))]
			[(cdr) (cdr (1st args))]
			[(caar) (caar (1st args))]
			[(cadr) (cadr (1st args))]
			[(cdar) (cdar (1st args))]
			[(cddr) (cddr (1st args))]
			[(caaar) (caaar (1st args))]
			[(caadr) (caadr (1st args))]
			[(cadar) (cadar (1st args))]
			[(cdaar) (cdaar (1st args))]
			[(caddr) (caddr (1st args))]
			[(cdadr) (cdadr (1st args))]
			[(cddar) (cddar (1st args))]
			[(cdddr) (cdddr (1st args))]
			[(list) (apply list args)]
			[(null?) (null? (1st args))]
      [(apply) (apply (lambda x (apply-proc (1st args) x)) (2nd args))] ;;; apply-proc requires its args to be a list
      [(map) (map (lambda x (apply-proc (1st args) x)) (2nd args))]
			; (assq obj alist)
			[(assq) (assq (1st args) (2nd args))]
			[(eq?) (eq? (1st args) (2nd args))]
			[(equal?) (equal? (1st args) (2nd args))]
			[(atom?) (atom? (1st args))]
			[(length) (length (1st args))]
			[(list->vector) (list->vector (1st args))]
			[(list?) (list? (1st args))]
			[(pair?) (pair? (1st args))]
			[(procedure?) (proc-val? (1st args))]
			[(vector->list) (vector->list (1st args))]
			[(vector) (apply vector args)]
			; (make-vector n)
			; (make-vector n obj)
			[(make-vector) (cond [(= (length args) 1) (make-vector (1st args))]
													 [else (make-vector (1st args) (2nd args))])]
			; (vector-ref vector n)
			[(vector-ref) (vector-ref (1st args) (2nd args))]
			[(vector?) (vector? (1st args))]
			[(number?) (number? (1st args))]
			[(symbol?) (symbol? (1st args))]
			; (set-car! pair obj)
			[(set-car!) (set-car! (1st args) (2nd args))]
			; (set-cdr! pair obj)
			[(set-cdr!) (set-cdr! (1st args) (2nd args))]
			; (vector-set! vector n obj)
			[(vector-set!) (vector-set! (1st args) (2nd args) (3rd args))]
			; (display obj)
			; (display obj textual-output-port)
			[(display) (cond [(= (length args) 1) (display (1st args))]
											 [else (display (1st args) (2nd args))])]
			; (newline)
			; (newline textual-output-port)
			[(newline) (cond [(= (length args) 1) (newline (1st args))]
											 [else (newline)])]
			[else (eopl:error 'apply-prim-proc
									 "Bad primitive procedure name: ~s" 
									 prim-proc)])))

;;; Syntax Expansion on the abstract syntax tree

(define syntax-expand
  (lambda (exp)
    (cases exp expression
        )
    ))

(define rep ; "read-eval-print" loop.
	(lambda ()
		(display "--> ")
		; notice that we don't save changes to the environment...
		(let ([answer (top-level-eval (parse-exp (read)))])
			; TODO: are there answers that should display differently?
			(eopl:pretty-print answer) (newline)
			(rep)))) ; tail-recursive, so stack doesn't grow.

(define eval-one-exp
	(lambda (x) (top-level-eval (parse-exp x))))

