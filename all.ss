
                                        ;:  Single-file version of the interpreter.
;; Easier to submit to server, probably harder to use in the development process

(load "chez-init.ss") 

                                        ;-------------------+
                                        ;                   |
                                        ;    DATATYPES      |
                                        ;                   |
                                        ;-------------------+

                                        ; parsed expression

;;; Define the predicate that checks whether a datum is a primitive type (or atomic)
(define (lit? datum)
  (not (pair? datum)))

(define (form? datum)
  (and (pair? datum)
       (eqv? (car datum) 'quote)))

(define (2-pair? lst)
  (equal? (length lst) 2))

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

;; environment type definitions

(define scheme-value?
  (lambda (x) #t))

(define-datatype environment environment?
  (empty-env-record)
  (extended-env-record
   (syms (list-of symbol?))
   (vals (list-of scheme-value?))
   (env environment?)))

                                        ; datatype for procedures.  At first there is only one
                                        ; kind of procedure, but more kinds will be added later.

(define-datatype proc-val proc-val?
  [prim-proc
   (name symbol?)]
  [closure (params (list-of symbol?))
           (bodies (list-of expression?))
           (env environment?)])



                                        ;-------------------+
                                        ;                   |
                                        ;    PARSER         |
                                        ;                   |
                                        ;-------------------+


                                        ; This is a parser for simple Scheme expressions, such as those in EOPL, 3.1 thru 3.3.

                                        ; You will want to replace this with your parser that includes more expression types, more options for these types, and error-checking.

                                        ; Procedures to make the parser a little bit saner.
(define 1st car)
(define 2nd cadr)
(define 3rd caddr)

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
          (cond ([null? body]
                 (eopl:error 'parse-exp "lambda expression missing body"))
                ([list? formals]
                 (if (andmap symbol? formals)
                     (lambda-exp (2nd datum)
                                 (map parse-exp (cddr datum)))
                     (eopl:error 'parse-exp
                                 "lambda argument list: formals must be symbols: ~s" formals)))
                ([symbol? formals]
                 (lambda-exp-variable (2nd datum)
                                      (map parse-exp (cddr datum))))))]

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
        (let* ([decls (2nd datum)]
               [vars (map car decls)]
               [exps (map cadr decls)]
               [length2? (lambda (x) (equal? 2 (length x)))])
          (cond ([< (length datum) 3] (eopl:error 'parse-exp
                                                  "let expression: incorrect length: ~s" datum))
                ([not (list? decls)] (eopl:error 'parse-exp
                                                 "decls: not a proper list: ~s" decls))
                ([not (andmap list? decls)] (eopl:error 'parse-exp
                                                        "decls: not all proper lists: ~s" decls))
                ([not (andmap length2? decls)] (eopl:error 'parse-exp
                                                           "let expression: decls: not all length 2: ~s" decls))
                ([not (andmap symbol? vars)] (eopl:error 'parse-exp
                                                         "decls: first members must be symbols: ~s" decls))
                (else (let-exp vars (map parse-exp exps) (map parse-exp (cddr datum))))))]

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
               [vars (map car decls)]
               [exps (map cadr decls)]
               [length2? (lambda (x) (equal? 2 (length x)))])
          (cond ([< (length datum) 3] (eopl:error 'parse-exp
                                                  "letrec expression: incorrect length: ~s" datum))
                ([not (list? decls)] (eopl:error 'parse-exp
                                                 "decls: not a proper list: ~s" decls))
                ([not (andmap list? decls)] (eopl:error 'parse-exp
                                                        "decls: not all proper lists: ~s" decls))
                ([not (andmap length2? decls)] (eopl:error 'parse-exp
                                                           "letrec expression: decls: not all length 2: ~s" decls))
                ([not (andmap symbol? vars)] (eopl:error 'parse-exp
                                                         "decls: first members must be symbols: ~s" decls))
                (else (letrec-exp vars (map parse-exp exps) (map parse-exp (cddr datum))))))]

       [(eqv? (1st datum) 'set!)
        (cond ([< (length datum) 3]
               (eopl:error 'parse-exp "set!: missing expression: ~s" datum))
              ([> (length datum) 3]
               (eopl:error 'parse-exp "set!: Too many parts: ~s" datum))
              (else (set-exp (parse-exp (2nd datum)) (parse-exp (3rd datum)))))]

       [else
        (if (list? datum)
            (app-exp (parse-exp (car datum)) (map parse-exp (cdr datum)))
            (eopl:error 'parse-exp
                        "application ~s is not a proper list" datum))])]
     [else (eopl:error 'parse-exp "bad expression: ~s" datum)])))




                                        ;-------------------+
                                        ;                   |
                                        ;   ENVIRONMENTS    |
                                        ;                   |
                                        ;-------------------+





                                        ; Environment definitions for CSSE 304 Scheme interpreter.  Based on EoPL section 2.3

(define empty-env
  (lambda ()
    (empty-env-record)))

(define extend-env
  (lambda (syms vals env)
    (extended-env-record syms vals env)))

(define list-find-position
  (lambda (sym los)
    (list-index (lambda (xsym) (eqv? sym xsym)) los)))

(define list-index
  (lambda (pred ls)
    (cond
     ((null? ls) #f)
     ((pred (car ls)) 0)
     (else (let ((list-index-r (list-index pred (cdr ls))))
             (if (number? list-index-r)
                 (+ 1 list-index-r)
                 #f))))))

(define apply-env
  (lambda (env sym succeed fail) ; succeed and fail are procedures applied if the var is or isn't found, respectively.
    (cases environment env
           (empty-env-record ()
                             (fail))
           (extended-env-record (syms vals env)
                                (let ((pos (list-find-position sym syms)))
                                  (if (number? pos)
                                      (succeed (list-ref vals pos))
                                      (apply-env env sym succeed fail)))))))






                                        ;-----------------------+
                                        ;                       |
                                        ;   SYNTAX EXPANSION    |
                                        ;                       |
                                        ;-----------------------+



                                        ; To be added later









                                        ;-------------------+
                                        ;                   |
                                        ;   INTERPRETER     |
                                        ;                   |
                                        ;-------------------+



                                        ; top-level-eval evaluates a form in the global environment

(define top-level-eval
  (lambda (form)
                                        ; later we may add things that are not expressions.
    (eval-exp form init-env)))

                                        ; eval-exp is the main component of the interpreter

(define eval-exp
  (lambda (exp env)
    (cases expression exp
           [lit-exp (datum) datum]
           [form-exp (datum) (2nd datum)]
           [var-exp (id)
                    (apply-env env id; look up its value.
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
           [lambda-exp (params bodies)
                       (closure params bodies env)]

           [else (eopl:error 'eval-exp "Bad abstract syntax: ~a" exp)])))

                                        ; evaluate the list of operands, putting results into a list

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
                                        ;  At this point, we only have primitive procedures.
                                        ;  User-defined procedures will be added later.

(define apply-proc
  (lambda (proc-value args)
    (cases proc-val proc-value
           [prim-proc (op) (apply-prim-proc op args)]
           [closure (params bodies env)
                    (let ([extended-env (extend-env params
                                                    args
                                                    env)])
                      (eval-bodies bodies extended-env))]
                                        ; You will add other cases
           [else (eopl:error 'apply-proc
                             "Attempt to apply bad procedure: ~s"
                             proc-value)])))

(define *prim-proc-names* '(+ - * / add1 sub1 zero? not = cons car cdr list
                              null? assq eq? equal? atom? length list->vector
                              list? pair? procedure? vector->list vector make-vector
                              vector-ref vector? number? symbol? set-car! set-cdr!
                              vector-set! display newline))

(define init-env         ; for now, our initial global environment only contains 
  (extend-env            ; procedure names.  Recall that an environment associates
   *prim-proc-names*   ;  a value (not an expression) with an identifier.
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

;;; prim-proc: symbols, args: scheme-values
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
      [else (eopl:error 'apply-prim-proc 
                   "Bad primitive procedure name: ~s" 
                   prim-proc)])))

(define rep      ; "read-eval-print" loop.
  (lambda ()
    (display "--> ")
    ;; notice that we don't save changes to the environment...
    (let ([answer (top-level-eval (parse-exp (read)))])
      ;; TODO: are there answers that should display differently?
      (eopl:pretty-print answer) (newline)
      (rep))))  ; tail-recursive, so stack doesn't grow.

(define eval-one-exp
  (lambda (x) (top-level-eval (parse-exp x))))
