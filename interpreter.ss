                                        ; -----------
                                        ; Interpreter
                                        ; -----------

                                        ; The top-level-eval evaluates a form in the global environment

(define top-level-eval
	(lambda (form)
                                        ; later we may add things that are not expressions.
		(eval-exp form (empty-env) (init-k))))

;;; The eval-exp is the main component of the interpreter
;; @param exp: expression dataype; env: envirionment datatype
;; @return scheme-value, prim-proc

(define (trace-all)
  (trace eval-rands
         apply-k
         eval-exp
         eval-bodies
         apply-proc))

(define apply-k
  (lambda (k val)
    (cases continuation k
           [init-k ()
                   (if (not (equal? val (void)))
                       val)]
           [rator-k (rands env k)
                    (eval-rands rands env (rands-k val k))]
           [rands-k (proc-value k)
                    (apply-proc proc-value val k)]
           [test-k (then-exp else-exp env k)
                   (if val
                       (eval-exp then-exp env k)
                       (eval-exp else-exp env k))]
           [test-k2 (then-exp env k)
                    (if val
                        (eval-exp then-exp env k)
                        (apply-k k (void)))]
           [extend-env-k (bodies k)
                         (eval-bodies bodies val k)]
           [define-k (var k)
             (extend-env (list var)
                         (list val)
                         init-env
                         (define-extend-k k))]
           [define-extend-k (k)
             (apply-k k (set! init-env val))]
           [eval-bodies-k (bodies env k)
                          (eval-bodies bodies env k)]
           [eval-rands-car-k (cdr-rands env k)
                             (eval-rands cdr-rands env
                                         (eval-rands-cdr-k val k))]
           [eval-rands-cdr-k (car-rands-val k)
                             (apply-k k (cons car-rands-val val))]
           [set-body-k (env var k)
                       (apply-env-ref env var
                                      (lambda (refer)
                                        (set-ref! refer val k))
                                      (lambda ()
                                        (apply-env-ref init-env var
                                                       (lambda (refer)
                                                         (set-ref! refer val k))
                                                       (lambda ()
                                                         (eopl:error 'set!
                                                                     "variable not found in environment: ~s" var)))))])))

;; eval-exp deals with the evaluation of special forms.
;; We leave the evaluation of procedures to eval-proc.
(define eval-exp   ;cps-version with DS continuations
	(lambda (exp env k)
		(cases expression exp
					 [lit-exp (datum) (apply-k k datum)]
					 [form-exp (datum) (apply-k k (2nd datum))]
					 [var-exp (id)
										(apply-env-ref env id ; look up its value.
                                   (lambda (v) (apply-k k (deref v)))
                                   (lambda ()
                                     (apply-env-ref init-env id
                                                    (lambda (v) (apply-k k (deref v)))
                                                    (lambda () (eopl:error 'apply-env ; procedure to call if id not in env
                                                                           "variable not found in environment: ~s"
                                                                           id)))))]
           [lambda-exp (params bodies)
											 (apply-k k (closure params bodies env))]

           [app-exp (rator rands)
                    (eval-exp rator env
                              (rator-k rands env k))]

           [if-then-else-exp (test-exp then-exp else-exp)
                             (eval-exp test-exp env
                                       (test-k then-exp else-exp env k))]

           [if-then-exp (test-exp then-exp)
                        (eval-exp test-exp env
                                  (test-k2 then-exp env k))]

                                        ;[while-exp (test bodies)
                                        ;           (letrec
                                        ;             ([helper
                                        ;                (lambda ()
                                        ;                  (if (eval-exp test env)
                                        ;                      (begin (eval-bodies bodies env) (helper))))])
                                        ;             (helper))]

           [set-exp (var body)
                    (eval-exp body env
                              (set-body-k env var k))]

           [define-exp (var exp)
             (eval-exp exp init-env
                       (define-k var k))]

					 [else (eopl:error 'eval-exp "Bad abstract syntax: ~a" exp)])))

;;; Evaluate the list of operands, putting results into a list

(define eval-rands
  (lambda (rands env k)
    (if (null? rands)
        (apply-k k '())
        (eval-exp (car rands) env
                  (eval-rands-car-k (cdr rands) env k)))))

(define eval-bodies
	(lambda (bodies env k)
		(let loop ([bodies bodies])
			(if (null? (cdr bodies))
					(eval-exp (car bodies) env k)
					(eval-exp (car bodies) env
                    (eval-bodies-k (cdr bodies) env k))))))

                                        ;  Apply a procedure to its arguments.

;;; Helper procedure that will allow extend-env bind varialbes in the correct fashion.
;;; > (imp-helper '(a b . c) '(1 2 3 4 5))
;;; (1 2 (3 4 5))
(define imp-helper
  (lambda (imp-ls ls)
    (if (pair? imp-ls)
        (cons (car ls) (imp-helper (cdr imp-ls) (cdr ls)))
        (list ls))))

;;; proc-val: proc-value datatype
;;; @return scheme-value
(define apply-proc
	(lambda (proc-value args k)
		(cases proc-val proc-value
					 [prim-proc (op) (apply-prim-proc op args k)]
					 [closure (params bodies env)
                    (cond
                     [(null? params)
                      (eval-bodies bodies env k)]

                     [(list? params)
                      (extend-env params args env
                                  (extend-env-k bodies k))]

                                        ; improper list
                     [(pair? params)
                      (extend-env (improper-list->proper params)
                                  (imp-helper params args)
                                  env
                                  (extend-env-k bodies k))]

                     [(symbol? params)
                      (extend-env (list params) (list args) env
                                  (extend-env-k bodies k))]

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
     (empty-env)
     (init-k))))

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
	(lambda (prim-proc args k)
    (apply-k k 
             (case prim-proc
               [(append) (apply append args)]
               [(eqv?) (apply eqv? args)]
               [(list-tail) (apply list-tail args)]
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
               [(apply) (apply-proc (car args) (cadr args) k)]
               [(map) (map (lambda x (apply-proc (1st args) x k)) (2nd args))]
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
               [(make-vector) (cond [(= (length args) 1) (make-vector (1st args))]
                                    [else (make-vector (1st args) (2nd args))])]
               [(vector-ref) (vector-ref (1st args) (2nd args))]
               [(vector?) (vector? (1st args))]
               [(number?) (number? (1st args))]
               [(symbol?) (symbol? (1st args))]
               [(set-car!) (set-car! (1st args) (2nd args))]
               [(set-cdr!) (set-cdr! (1st args) (2nd args))]
               [(vector-set!) (vector-set! (1st args) (2nd args) (3rd args))]
               [(display) (cond [(= (length args) 1) (display (1st args))]
                                [else (display (1st args) (2nd args))])]
               [(newline) (cond [(= (length args) 1) (newline (1st args))]
                                [else (newline)])]
               [(quotient) (quotient (1st args) (2nd args))]
               [else (eopl:error 'apply-prim-proc
                                 "Bad primitive procedure name: ~s" 
                                 prim-proc)]))))

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
                                        ;[while-exp (test bodies)
                                        ;           (while-exp (syntax-expand test) (map syntax-expand bodies))]

           [named-let-exp (name vars exps bodies)
                          (app-exp
                           (syntax-expand
                            (letrec-exp (list name)   ; proc-names
                                        (list
                                         (lambda-exp vars
                                                     (map syntax-expand bodies))) ; bodiess
                                        (list (var-exp name))))
                           exps)]

           [letrec-exp (proc-names lambdas letrec-bodies)
                       (let ([lambdas (map syntax-expand lambdas)]
                             [letrec-bodies (map syntax-expand letrec-bodies)])
                         (letrec ([helper
                                   (lambda (proc-names lambdas letrec-bodies)
                                     (if (null? proc-names)
                                         (list (let-exp '()
                                                        '()
                                                        letrec-bodies))
                                         (cons (set-exp (car proc-names)
                                                        (car lambdas))
                                               (helper (cdr proc-names)
                                                       (cdr lambdas)
                                                       letrec-bodies))))])
                           (syntax-expand
                            (let-exp proc-names ; var
                                     (map (lambda (x) (lit-exp #f)) proc-names) ; #f
                                     (helper proc-names lambdas letrec-bodies)))))]

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

