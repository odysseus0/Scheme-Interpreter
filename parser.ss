; ------
; Parser
; ------

(define 1st car)
(define 2nd cadr)
(define 3rd caddr)
(define 4th cadddr)

(define improper-list?
  (lambda (datum)
    (and (pair? datum)
         (not (list? datum)))))

(define improper-list->proper
  (lambda (impLst)
    (if (atom? impLst)
        (list impLst)
        (cons (car impLst) (improper-list->proper (cdr impLst))))))

(define proper-list->improper
  (lambda (propLst)
    (if (= (length propLst) 1)
        (car propLst)
        (cons (car propLst) (proper-list->improper (cdr propLst))))))

(define parse-exp
	(lambda (datum)
		(cond
		 [(symbol? datum) (var-exp datum)]
		 [(form? datum) (form-exp datum)]
		 [(lit? datum) (lit-exp datum)]
		 [(pair? datum)
			(cond
			 [(eqv? (1st datum) 'lambda)
				(let ([body (cddr datum)]
							[formals (2nd datum)])
					(cond ([null? body] (eopl:error 'parse-exp "lambda expression missing body"))
                ([symbol? formals] (lambda-exp formals (map parse-exp (cddr datum))))
                ([list? formals]
                 (if (andmap symbol? formals)
                     (lambda-exp formals (map parse-exp (cddr datum)))
                     (eopl:error 'parse-exp "lambda argument list: formals must be symbols: ~s" formals)))
                ([improper-list? formals]
                 (lambda-exp formals (map parse-exp (cddr datum))))))]

			 [(eqv? (1st datum) 'if)
				(cond ([= (length datum) 3]
							 (if-then-exp (parse-exp (2nd datum)) (parse-exp (3rd datum))))
							([= (length datum) 4]
							 (if-then-else-exp (parse-exp (2nd datum))
																 (parse-exp (3rd datum))
																 (parse-exp (cadddr datum))))
							(else
							 (eopl:error 'parse-exp
													 "if: incorrect number of arguments: ~s" datum)))]

			 ;; (let ((var expr) ...) body1 body2 ...) 
			 [(eqv? (1st datum) 'let)
        ;; Named let
        ;; (let name ((var expr) ...) body1 body2 ...)
        (if (symbol? (2nd datum))
            (named-let-exp (2nd datum)
                           (map car (3rd datum))
                           (map (lambda (x) (parse-exp (cadr x))) (3rd datum))
                           (map parse-exp (cdddr datum)))
            (let* ([decls (2nd datum)]
							     [vars (map car decls)]
							     [exps (map cadr decls)]
							     [length2? (lambda (x) (equal? 2 (length x)))])
              (cond
                ([< (length datum) 3] (eopl:error 'parse-exp
																									"let expression: incorrect length: ~s" datum))
								([not (list? decls)] (eopl:error 'parse-exp
																								 "decls: not a proper list: ~s" decls))
								([not (andmap list? decls)] (eopl:error 'parse-exp
																												"decls: not all proper lists: ~s" decls))
								([not (andmap length2? decls)] (eopl:error 'parse-exp
																													 "let expression: decls: not all length 2: ~s" decls))
								([not (andmap symbol? vars)] (eopl:error 'parse-exp
																												 "decls: first members must be symbols: ~s" decls))
								(else (let-exp vars (map parse-exp exps) (map parse-exp (cddr datum)))))))]

			 [(eqv? (1st datum) 'let*)
				(let* ([decls (2nd datum)]
							 [vars (map car decls)]
							 [exps (map cadr decls)]
							 [length2? (lambda (x) (equal? 2 (length x)))])
					(cond ([< (length datum) 3] (eopl:error 'parse-exp
																									"let* expression: incorrect length: ~s" datum))
								([not (list? decls)] (eopl:error 'parse-exp
																								 "decls: not a proper list: ~s" decls))
								([not (andmap list? decls)] (eopl:error 'parse-exp
																												"decls: not all proper lists: ~s" decls))
								([not (andmap length2? decls)] (eopl:error 'parse-exp
																													 "let* expression: decls: not all length 2: ~s" decls))
								([not (andmap symbol? vars)] (eopl:error 'parse-exp
																												 "decls: first members must be symbols: ~s" decls))
								(else (let*-exp vars (map parse-exp exps) (map parse-exp (cddr datum))))))]

			 [(eqv? (1st datum) 'letrec)
				(let* ([decls (2nd datum)]
							 [proc-names (map 1st decls)]
               [lambdas (map (lambda (x) (parse-exp (2nd x))) decls)]
               [letrec-bodies (map parse-exp (cddr datum))]
							 [length2? (lambda (x) (equal? 2 (length x)))])
					(cond ([< (length datum) 3] (eopl:error 'parse-exp
																									"letrec expression: incorrect length: ~s" datum))
								([not (list? decls)] (eopl:error 'parse-exp
																								 "decls: not a proper list: ~s" decls))
								([not (andmap list? decls)] (eopl:error 'parse-exp
																												"decls: not all proper lists: ~s" decls))
								([not (andmap length2? decls)] (eopl:error 'parse-exp
																													 "letrec expression: decls: not all length 2: ~s" decls))
								([not (andmap symbol? proc-names)] (eopl:error 'parse-exp
																												 "decls: first members must be symbols: ~s" decls))
								(else (letrec-exp proc-names lambdas letrec-bodies))))]

			 [(eqv? (1st datum) 'set!)
				(cond ([< (length datum) 3]
							 (eopl:error 'parse-exp "set!: missing expression: ~s" datum))
							([> (length datum) 3]
							 (eopl:error 'parse-exp "set!: Too many parts: ~s" datum))
							(else (set-exp (2nd datum) (parse-exp (3rd datum)))))]

       [(eqv? (1st datum) 'and)
        (and-exp (map parse-exp (cdr datum)))]

       [(eqv? (1st datum) 'or)
        (or-exp (map parse-exp (cdr datum)))]

       [(eqv? (1st datum) 'cond)
        (cond-exp (map (lambda (x)
                        (if (eqv? (car x) 'else)
                            (list (var-exp 'else) (map parse-exp (cdr x)))
                            (list (parse-exp (car x)) (map parse-exp (cdr x)))))
                       (cdr datum)))]

       [(eqv? (1st datum) 'case)
        (case-exp (parse-exp (2nd datum))
                  (map (lambda (clause)
                         (if (eqv? (car clause) 'else)
                             (list (var-exp 'else) (map parse-exp (cdr clause)))
                             (list (parse-exp (apply list (cons 'list (car clause)))) (map parse-exp (cdr clause)))))
                       (cddr datum)))]

       [(eqv? (1st datum) 'begin)
        (begin-exp (map parse-exp (cdr datum)))]

       [(eqv? (1st datum) 'while)
        (while-exp (parse-exp (2nd datum)) (map parse-exp (cddr datum)))]

       [(eqv? (1st datum) 'define)
        (define-exp (2nd datum) (parse-exp (3rd datum)))]

			 [else
				(if (list? datum)
						(app-exp (parse-exp (car datum)) (map parse-exp (cdr datum)))
						(eopl:error 'parse-exp
												"application ~s is not a proper list" datum))])]

		 [else (eopl:error 'parse-exp "bad expression: ~s" datum)])))


;;; Unparser for debugging purpose
(define unparse-exp
  (lambda (exp)
    (cases expression exp
           [var-exp (id) id]
           [lit-exp (id) id]
           [form-exp (form) `,form]
           [lambda-exp (formals body)
                       `( lambda ,formals
                          ,@(map unparse-exp body))]
           ;[lambda-exp-variable (formal body)
           ;                     `( lambda ,formal
           ;                        ,@(map unparse-exp body))]
           ;[lambda-exp-improper (formals body)
           ;                     `( lambda ,(proper-list->improper formals)
           ;                        ,@(map unparse-exp body))]
           [if-then-exp (pred then-exp)
                        `( if ,(unparse-exp pred) ,(unparse-exp then-exp))]
           [if-then-else-exp (pred then-exp else-exp)
                   `( if ,(unparse-exp pred) ,(unparse-exp then-exp) ,(unparse-exp else-exp))]
           [let-exp (vars exps body)
                    `( let ,(map (lambda (x y) (list x (unparse-exp y))) vars exps) ,@(map unparse-exp body))]
           [let*-exp (vars exps body)
                     `( let* ,(map (lambda (x y) (list x (unparse-exp y))) vars exps) ,@(map unparse-exp body))]
           ;[letrec-exp (proc-names idss bodiess letrec-bodies)
           ;            `( letrec ,(map (lambda (x y) (list x (unparse-exp y))) vars exps) ,@(map unparse-exp body))]
           [set-exp (var body)
                    `( set! ,var ,(unparse-exp body))]
           [and-exp (body)
                    `( and ,@(map unparse-exp body))]
           [or-exp (body)
                   `( or ,@(map unparse-exp body))]
           ;[cond-exp (clauses)
           ;[case-exp (expr clauses)
           [begin-exp (bodies)
                      `( begin ,@(map unparse-exp rest))]
           [app-exp (rator rand)
                    `( ,(unparse-exp rator) ,@(map unparse-exp rand))]
           [else (eopl:error 'unparse-exp "Cannot unparse expression ~s. Unspecified" exp)])))


