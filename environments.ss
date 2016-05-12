; -----------
; Environment
; -----------

(define empty-env
	(lambda ()
		(empty-env-record)))

(define extend-env
	(lambda (syms vals env)
		(extended-env-record syms (list->vector vals) env)))

(define extend-env-recursively
  (lambda (proc-names idss bodiess old-env)
    (recursively-extended-env-record
     proc-names idss bodiess old-env)))

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

(define cons-vector
  (lambda (exp vec)
    (list->vector (cons exp (vector->list vec)))))

(define add-to-env
  (lambda (sym val env)
    (cases environment env
           (extended-env-record (syms vals env)
                                (begin (set-cdr! syms syms)
                                       (set-cdr! vals vals)
                                       (set-car! syms sym)
                                       (set-car! vals val)))
           (else env))))

(define apply-env
	(lambda (env sym succeed fail) ; succeed and fail are procedures applied
                                 ; if the var is or isn't found, respectively.
    (deref (apply-env-ref env sym succeed fail))))

(define apply-env-ref
  (lambda (env sym succeed fail)
		(cases environment env
					 (empty-env-record ()
														 (fail))
					 (extended-env-record (syms vals env)
																(let ((pos (list-find-position sym syms)))
																	(if (number? pos)
                                      (succeed (refer vals pos))
																			(apply-env-ref env sym succeed fail))))
           [recursively-extended-env-record
            (proc-names idss bodiess old-env)
            (let ([pos (list-find-position sym proc-names)])
              (if (number? pos)
                  (succeed (refer (vector (closure (list-ref idss pos)
                                    (list-ref bodiess pos)
                                    env)) 0))
                  (apply-env-ref old-env sym succeed fail)))])))

