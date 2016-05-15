;; ---------- Core Form: Basic error checking and simplification.

(define *prim-names*
  '(+ - * / = < boolean? car cdr char? char->integer cons eq?
      integer? string->uninterned-symbol not null? pair? procedure?
      string string? string-length string-ref
      vector vector? vector-length vector-ref
      vector-set! symbol? symbol->string))

(define *keywords*
  '(quote begin if set! lambda))

(define core-form
  (lambda (exp)
    (core-convert exp)))

(define core-convert
  (lambda (exp)
    (if (not (pair? exp))
        (cond
         [(symbol? exp) exp]
         [(or (number? exp) (boolean? exp) (string? exp) (char? exp))
          exp]
         [else
          (error 'core-convert "Bad expression ~s" exp)])
        (record-case exp
          [quote (obj)
                 `(quote ,obj)]
          [begin (e0 . exps)
                 (if (null? exps)
                     (core-convert e0)
                     (let ([new-e0 (core-convert e0)]
                           [new-e1 (core-convert `(begin . ,exps))])
                       `(begin ,new-e0 ,new-e1)))]
          [if (t c a)
              (let ([new-t (core-convert t)]
                    [new-c (core-convert c)]
                    [new-a (core-convert a)])
                `(if ,new-t ,new-c ,new-a))]
          [set! (v e)
            (cond
             [(not (symbol? v))
              (error 'core-convert "Bad expression ~s" exp)]
             [else
              (let ([new-e (core-convert e)])
                `(set! ,v ,new-e))])]
          [lambda (formals . bodies)
            (if (not (and (list? formals)
                          (andmap symbol? formals)
                          (andmap (lambda (x) (not (memq x *keywords*)))
                                  formals)
                          (set? formals)))
                (error 'core-convert "Bad formals ~s in ~s" formals exp)
                (let ([new-body (core-convert `(begin ,@bodies))])
                  `(lambda ,formals ,new-body)))]
          [let (decls . bodies)
            (let ([vars (map car decls)]
                  [vals (map cadr decls)])
              (core-convert `((lambda ,vars ,@bodies) ,@vals)))]

          [let* (decls . bodies)
            (display (cdr decls))
            (newline)
            (display bodies)
            (newline)]

          [let** (decls . bodies)
                 (if (null? decls)
                     `(let () ,@bodies)
                     (let ([new-decls `(,(car decls))]
                           [new-bodies (core-convert `(let* ,(cdr decls) ,@bodies))])
                       `(let ,new-decls ,new-bodies)))]

          [letrec (decls . bodies)
            (let ([vars (map car decls)]
                  [vals (map cadr decls)])
              (let ([holders (map (lambda (x) #f) vars)]
                    [assigns (map (lambda (v e) `(set! ,v ,e)) vars vals)])
                (core-convert
                 `((lambda ,vars ,@assigns ,@bodies) ,@holders))))]
          [else
           (if (or (null? exp)
                   (not (list? exp))
                   (memq (car exp) *keywords*))
               (error 'core-convert "Bad expression ~s" exp)
               (let ([rator (car exp)]
                     [rands (cdr exp)])
                 (let ([new-rator (core-convert rator)]
                       [new-rands (core-convert-list rands)])
                   `(,new-rator . ,new-rands))))]))))

(define core-convert-list
  (lambda (ls)
    (map core-convert ls)))

;; ---------- Utility procedures

(define list-index
  (lambda (v ls)
    (let loop ([ls ls] [acc 0])
      (cond
       [(null? ls) #f]
       [(eq? (car ls) v) acc]
       [else (loop (cdr ls) (add1 acc))]))))

(define union
  (lambda (a b)
    (cond
     [(null? a) b]
     [(memq (car a) b) (union (cdr a) b)]
     [else (cons (car a) (union (cdr a) b))])))

(define difference
  (lambda (a b)
    (cond
     [(null? a) '()]
     [(memq (car a) b) (difference (cdr a) b)]
     [else (cons (car a) (difference (cdr a) b))])))

(define intersection
  (lambda (a b)
    (cond
     [(null? a) '()]
     [(memq (car a) b) (cons (car a) (intersection (cdr a) b))]
     [else (intersection (cdr a) b)])))

(define unit-set
  (lambda (item)
    (list item)))

(define set?
  (lambda (ls)
    (or (null? ls)
        (and (not (memq (car ls) (cdr ls)))
             (set? (cdr ls))))))
