; -----------
; Interpreter
; -----------

; The top-level-eval evaluates a form in the global environment

(define top-level-eval
	(lambda (form)
    ; later we may add things that are not expressions.
		(eval-exp form (empty-env))))

;;; params are the parameters in the closure datatype. It can be
;; both symbol and references.
;;; args here are unevaluated arguments. 
(define eval-args
  (lambda (params args env)
    (cond [(null? params) (list)]
          [(symbol? (car params)) (cons (eval-exp (car args) env)
                                        (eval-args (cdr params) (cdr args) env))]
          [else (cons (apply-env-ref env (car args) (lambda (x) x) (std-fail (car args)))
                      (eval-args (cdr params) (cdr args) env))])))

;;; The eval-exp is the main component of the interpreter
;; @param exp: expression dataype; env: envirionment datatype
;; @return scheme-value, prim-proc

;; eval-exp deals with the evaluation of special forms.
;; We leave the evaluation of procedures to eval-proc.
(define eval-exp
	(lambda (exp env)
		(cases expression exp
					 [lit-exp (datum) datum]
					 [form-exp (datum) (2nd datum)]
					 [var-exp (id)
										(apply-env env id ; look up its value.
															 (lambda (x) x) ; procedure to call if id is in the environment 
                               (std-fail id))]

           [letrec-exp (proc-names idss bodiess letrec-bodies)
                       (eval-bodies letrec-bodies
                                    (extend-env-recursively proc-names idss bodiess env))]

					 [let-exp (vars exps bodies)
										(let ([extended-env (extend-env vars
																										(eval-rands exps env)
																										env)])
											(eval-bodies bodies extended-env))]

					 [app-exp (rator rands)
										(let ([proc-value (eval-exp rator env)])
                      (cases proc-val proc-value
                             [prim-proc (op) (apply-proc proc-value (eval-rands rands env))]
                             [closure (params bodies new-env)
                                      (if (list? params)
                                          (apply-proc proc-value
                                                      (eval-args params rands env))
                                          (apply-proc proc-value (eval-rands rands env)))]))]

					 [if-then-else-exp (test-exp then-exp else-exp)
														 (if (eval-exp test-exp env)
																 (eval-exp then-exp env)
																 (eval-exp else-exp env))]
           [if-then-exp (test-exp then-exp)
                        (if (eval-exp test-exp env)
                            (eval-exp then-exp env))]
					 [lambda-exp (params bodies)
											 (closure params bodies env)]
           [while-exp (test bodies)
                      (letrec
                        ([helper
                           (lambda ()
                             (if (eval-exp test env)
                                 (begin (eval-bodies bodies env) (helper))))])
                        (helper))]

           [do2-exp (bodies test)
                    (begin (eval-bodies bodies env)
                           (eval-exp (while-exp test bodies) env))]

           [call-with-values-exp (producer consumer)
                                 (let* ([producer (eval-exp producer env)]
                                        [args (apply-proc producer (list))]
                                        [consumer (eval-exp consumer env)])
                                   (apply-proc consumer args))]

           [set-exp (var body)
                    (let ([body-val (eval-exp body env)])
                      (apply-env-ref env var
                                     (lambda (refer)
                                       (set-ref! refer body-val))
                                     (lambda ()
                                       (apply-env-ref init-env var
                                                      (lambda (refer)
                                                        (set-ref! refer body-val))
                                                      (lambda () (eopl:error 'set! ; procedure to call if id not in env
                                                                             "variable not found in environment: ~s"
                                                                             var))))))]


           [define-exp (var exp)
             (set! init-env (extend-env (list var) (list (eval-exp exp init-env)) init-env))]


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

;;; Helper procedure that will allow extend-env bind varialbes in the correct fashion.
;;; > (imp-helper '(a b . c) '(1 2 3 4 5))
;;; (1 2 (3 4 5))
(define imp-helper
  (lambda (imp-ls ls)
    (if (pair? imp-ls)
      (cons (car ls) (imp-helper (cdr imp-ls) (cdr ls)))
      (list ls))))

; > (normalize-params '(a (ref x)))
; (a x)
(define normalize-params
  (lambda (params)
    (cond [(null? params) (list)]
          [(ref? (car params)) (cons (cadr (car params)) (normalize-params (cdr params)))]
          [else (cons (car params) (normalize-params (cdr params)))])))

;;; proc-val: proc-value datatype
;;; @return scheme-value
(define apply-proc
	(lambda (proc-value args)
		(cases proc-val proc-value
					 [prim-proc (op) (apply-prim-proc op args)]
					 [closure (params bodies env)
                    (cond
                     [(null? params)
                      (eval-bodies bodies env)]

                     [(list? params)
                      (let ([extended-env (extend-env (normalize-params params)
                                                      args
                                                      env)])
                        (eval-bodies bodies extended-env))]

                      ; improper list
                     [(pair? params)
                      (let ([extended-env (extend-env (improper-list->proper params)
                                                      (imp-helper params args)
                                                      env)])
                        (eval-bodies bodies extended-env))]
                     [(symbol? params)
                      (let ([extended-env (extend-env (list params)
                                                      (list args)
                                                      env)])
                        (eval-bodies bodies extended-env))]
                     [else (eopl:error 'apply-proc
                                       "Attempt to apply bad procedure: ~s"
                                       proc-value)])])))

(define *prim-proc-names*
	'(+ - * / add1 sub1 zero? not = < > <= >= cons car cdr
		caar cadr cdar cddr caaar caadr cadar cdaar caddr cdadr
		cddar cdddr list null? assq eq? equal? atom? length
		list->vector list? pair? procedure? vector->list
		vector make-vector vector-ref vector? number? symbol?
		set-car! set-cdr! vector-set! display newline procedure?
    apply map quotient memv values call-with-values list-tail eqv? append))

(define generate-init-env
  (lambda ()
    (extend-env
      *prim-proc-names*
      (map prim-proc *prim-proc-names*)
      (empty-env))))

(define init-env
  (generate-init-env))

(define reset-global-env
  (lambda ()
    (set! init-env (generate-init-env))))

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
      [(append) (apply append args)]
      [(eqv?) (apply eqv? args)]
      [(list-tail) (apply list-tail args)]
      [(values) args] ; package the values as a list
      [(call-with-values) (apply-proc (cadr args) (apply-proc (car args) '()))]
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
      [(apply) (apply-proc (car args) (cadr args))]
      [(map) (map (lambda x (apply-proc (1st args) x)) (2nd args))]
      [(memv) (memv (1st args) (2nd args))]
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
      [(quotient) (quotient (1st args) (2nd args))]
			[else (eopl:error 'apply-prim-proc
									 "Bad primitive procedure name: ~s" 
									 prim-proc)])))

;;; Syntax Expansion on the abstract syntax tree

(define syntax-expand
  (lambda (exp)
    (cases expression exp
           [if-then-exp (pred then-exp)
                        (let ([pred (syntax-expand pred)]
                              [then-exp (syntax-expand then-exp)])
                          (if-then-exp pred then-exp))]

           [if-then-else-exp (pred then-exp else-exp)
                             (let ([pred (syntax-expand pred)]
                                   [then-exp (syntax-expand then-exp)]
                                   [else-exp (syntax-expand else-exp)])
                               (if-then-else-exp pred then-exp else-exp))]

           [let-exp (vars exps bodies)

                    (let ([bodies (map syntax-expand bodies)]
                          [exps (map syntax-expand exps)])
                      (app-exp (lambda-exp vars bodies) exps))]
           [let*-exp (vars exps bodies)
                     (let ([bodies (map syntax-expand bodies)]
                           [exps (map syntax-expand exps)])
                       (cond [(null? vars) bodies] ; I may change "bodies" to a begin expression later
                             [(null? (cdr vars))
                              (syntax-expand (let-exp (list (car vars)) (list (car exps)) bodies))]
                             [else 
                              (syntax-expand (let-exp (list (car vars)) (list (car exps))
                                                      (list (syntax-expand (let*-exp (cdr vars) (cdr exps) bodies)))))]))]
           [begin-exp (bodies)
                      (app-exp (lambda-exp (list) (map syntax-expand bodies)) (list))]
           [and-exp (exps)
                    (let ([exps (map syntax-expand exps)])
                      (cond [(null? exps) (lit-exp #t)]
                            [(null? (cdr exps)) (car exps)]
                            [else
                             (if-then-else-exp (car exps)
                                               (syntax-expand (and-exp (cdr exps)))
                                               (lit-exp #f))]))]
           [or-exp (exps)
                   (let ([exps (map syntax-expand exps)])
                     (cond [(null? exps) (lit-exp #f)]
                           [(null? (cdr exps)) (car exps)]
                           [else
                            (syntax-expand
                              (let-exp (list 't)
                                       (list (car exps))
                                       (list (if-then-else-exp (var-exp 't) (var-exp 't)
                                                               (syntax-expand (or-exp (cdr exps)))))))]))]

           [cond-exp (clauses)
                     (let* ([test (syntax-expand (1st (1st clauses)))]
                            [exps (map syntax-expand (2nd (1st clauses)))]
                            [rest-clauses (cdr clauses)]
                            [bodies (syntax-expand (begin-exp exps))])
                       (if (null? rest-clauses)
                           (if (equal? test (var-exp 'else))
                               bodies
                               (if-then-exp test
                                            bodies))
                           (if-then-else-exp test
                                             bodies
                                             (syntax-expand (cond-exp rest-clauses)))))]

           [case-exp (expr clauses)
                     (let* ([expr (syntax-expand expr)]
                            [keys (1st (1st clauses))]
                            [exps (map syntax-expand (2nd (1st clauses)))]
                            [bodies (syntax-expand (begin-exp exps))]
                            [rest-clauses (cdr clauses)]
                            [test (app-exp (parse-exp 'memv) (list expr keys))])
                       (if (null? rest-clauses)
                           (if (equal? keys (var-exp 'else))
                               bodies
                               (if-then-exp test
                                            bodies))
                           (if-then-else-exp test
                                             bodies
                                             (syntax-expand (case-exp expr rest-clauses)))))]

           [while-exp (test bodies)
                      (while-exp (syntax-expand test) (map syntax-expand bodies))]
           [do1-exp (bodies test)
                    (syntax-expand (begin-exp (snoc bodies (while-exp test bodies))))]
           [do2-exp (bodies test)
                    (do2-exp (map syntax-expand bodies) (syntax-expand test))]

           [named-let-exp (name vars exps bodies)
                          (app-exp
                            (letrec-exp (list name)   ; proc-names
                                        (list vars)   ; idss
                                        (list (map syntax-expand bodies)) ; bodiess
                                        (list (var-exp name)))
                            exps)]

           [letrec-exp (proc-names idss bodiess letrec-bodies)
                       (let ([bodiess (map (lambda (bodies) (map syntax-expand bodies)) bodiess)]
                             [letrec-bodies (map syntax-expand letrec-bodies)])
                         (letrec-exp proc-names idss bodiess letrec-bodies))]

           [define-exp (var exp)
                       (define-exp var (syntax-expand exp))]

           [lambda-exp (formals bodies)
                       (lambda-exp formals (map syntax-expand bodies))]

           [else exp])))

(define rep ; "read-eval-print" loop.
	(lambda ()
		(display "--> ")
		; notice that we don't save changes to the environment...
		(let ([answer (top-level-eval (syntax-expand (parse-exp (read))))])
			; TODO: are there answers that should display differently?
			(eopl:pretty-print answer) (newline)
			(rep)))) ; tail-recursive, so stack doesn't grow.

(define eval-one-exp
	(lambda (x) (top-level-eval (syntax-expand (parse-exp x)))))

