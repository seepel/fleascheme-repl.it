(define tests #f)
(define trace '())

(define-macro (test result expected)
  (if tests
    `(let ((result ,result)
           (expected ,expected)) 
       (if (equal? ,result ,expected)
         (begin 
           (display (string-append "Test Succeeded\n"))
           (print (quote ,result)))
         (begin
           (display (string-append "Test Failed\n"))
           (print (quote ,result))
           (display "Expected:\n")
           (print result)
           (display "Got:\n")
           (print expected))))
    (begin)))

(define-macro (dlet* bindings . body)
  (cond
    ((null? bindings)
     `(let () ,@body))
    ((pair? bindings)
     (let* ((binding (car bindings))
            (bindings (cdr bindings)))
       (cond
         ((symbol? (car binding))
          `(let (,binding)
             (dlet* ,bindings ,@body)))
         ((pair? (car binding))
          (let ((p (gensym 'p)))
            `(let ((,p ,(cadr binding)))
               (dlet* ((,(caar binding) (car ,p)))
                 ,(if (null? (cdar binding))
                   `(dlet* ,bindings ,@body)
                   `(dlet* ((,(cdar binding) (cdr ,p))
                            ,@bindings)
                      ,@body))))
                 )))))))

(test (dlet* () 'foo) 'foo)
(test (dlet* ((a '(1 2 3))) a) '(1 2 3))
(test (dlet* (((a . b) '(1 2 3))) a) 1)
(test (dlet* (((a . b) '(1 2 3))) b) '(2 3))
(test (dlet* (((a b . c) '(1 2 3))) a) 1)
(test (dlet* (((a b . c) '(1 2 3))) b) 2)
(test (dlet* (((a b . c) '(1 2 3))) c) '(3))
(test (dlet* (((a b c) '(1 2 3))) a) 1)
(test (dlet* (((a b c) '(1 2 3))) b) 2)
(test (dlet* (((a b c) '(1 2 3))) c) 3)


(define (assert x . messages)
  (unless x
    (raise (string-append messages))))

(define (error message . args)
  ;; FIXME: Handle formats (~a ~s . rest)
  (raise message))
 
(define-macro (free x) `(set! ,x #f))

(define-macro (mif be te ee)
   (let ((vs 
         ;;(car (freevars be '() '()))
         ))
     `(if-function (lambda () ,be)
        (lambda ,vs ,te)
        (lambda ,vs ,ee))))
 
(define (if-function be te ee)
  (dlet* (((pval . stuff) (be)))
    (if pval 
      (apply te stuff)
      (apply ee stuff))))

(define-macro (mcond . clauses)
  (dlet* ((((be . rest) . clauses) clauses))
    (if (eq? be 'else)
      `(begin ,@rest)
      `(mif ,be 
         (begin ,@rest)
         (mcond ,@clauses)))))

(define (mcopy x)
  "Pedagogical non-primitive copy function."
  (mif (matom? x) 
    `(,x ,@x)
    (dlet* (((carx . cdrx) x)
            ((ncar1 . ncar2) (mcopy carx))
            ((ncdr1 . ncdr2) (mcopy cdrx)))
      `((,ncar1 ,@ncdr1) . (,ncar2 ,@ncdr2)))))

(define (matom? x)
  ;; FIXME: Other types?
  `(,(or (symbol? x)
         (procedure? x)
         (number? x)
         (string? x)
         (null? x))
    ,x)) 

(define (meq? x y) `(,(eq? x y) ,x))

(define (meq2? x y) `(,(eq? x y) ,x ,y))

(define (msymbol? x) `(,(symbol? x) ,x))

(define (mnull? x) `((,null? x) ,x))

(define (massoc x e)
  "return binding, else #f"
  (mif (mnull? e)
    (begin
      (free x)
      `(#f ,@e))
    (dlet* ((((var . val) . reste) e))
      (mif (meq2? x var)
        (begin
          (free x)
          `((,var ,@val) ,@reste))
        (dlet* (((nbinding . nreste) (massoc x reste)))
          `(,nbinding . ((,var ,@val) ,@reste)))))))

(define (meval-begin xl e)
  (dlet* (((x . xl) xl))
    (mif (mnull? xl)
      (begin
        (assert (null? xl))
        (meval x e))
      (dlet* (((xval . ne) (meval x e))
              ((xlval . nne) (meval-begin xl ne)))
        (free xval)
        `(,xlval ,@nne)))))

(define (meval x e)
  (mcond
    ((msymbol? x)
     (dlet* ((((var .val) . ne) (massoc x e)))
       (free var)
       `(,val ,@ne)))
    ((matom? x) 
     `(,x ,@e))
    (else ; better be a pair
     (dlet* (((fn . args) x)) ; oh hey, there was a typo here in the paper (missing begin paren in the bindings)
       (mcond
         ((meq? fn 'begin)
          (free fn)
          (meval-begin args e))
         ((meq? fn 'function)
          (free fn)
          (dlet* (((lambda) args)
                  ((nlambda . lambda) 
                   (mcopy lambda))
                  ((fvars . bvars) 
                   (freevars `(lambda ,nlambda) '() '()))
                  ((e1 . e2) (split fvars e)))
           ;; TODO: decide on funarg vs closure
           `((funarg ,lambda ,e1) ,@e2)))
         (else ; procedure application
          ;; This is different because scheme is a lisp-1 and linear lisp is a lisp-2
          (dlet* ((nfn . ne) (meval fn e)
                  ((nargs . nne) (mevlis args( ne)))
            `(,(mapply nfn nargs ,@nne))))))))))

(define (mapply fn args)
  (mif (matom? fn) ; FIXME: this means it's a host procedure
    (apply fn args)
    (dlet* (((ffn . rfn) fn))
      (mcond
        ((meq? ffn 'function)
         (free ffn)
         (dlet* (((bvlist . body) rfn)
                 ((v . ne) (meval-begin body (mpairlis bvlist args '()))))
           (assert (null? ne))
           v))
        ((meq? ffn 'funarg)
         (free ffn)
         (dlet* ((((lambda bvlist . body) ce) rfn)
                 ((v . ne) (meval-begin body (mpairlis bvlist args ce))))
           (free lambda)
           (assert (null? ne)
           v))
        (else
         (error "mapply: bad fn ~S" fn)))))))

(define (mpairlis vars vals e)
  (mif (mnull? vars)
    (begin
      (assert (null? vars))
      e)
    (dlet* (((var . vars) vars)
            ((val . vals) vals))
      `(,var ,@val) ,@(mpairlis vars vals e))))

(define (mevlis args e)
  (mif (mnull? args)
    (begin
      (assert (null? args))
      `(() ,@e))
    (dlet* (((x . args) args)
            ((xval . e) (meval x e))
            ((argvals . e) (mevlis args e)))
      `((,xval ,@argvals) ,@e))))

(define (split vars e)
  (mif (mnull? args)
    (begin
      (assert (null? vars)
      (`() ,@e)))
    (dlet* (((var . nvars) vars)
            ((binding . ne) (massoc var e))
            ((e1 . e2) (split nvars ne)))
      `((,binding ,@e1) ,@e2))))

(define (mmember? x ls)
  (mif (mnull? ls)
    (begin
      (free x)
      `(() ,@ls))
    (dlet* (((carls . ls) ls))
      (mif (meq2? x carls)
        (begin
          (free x)
          `(,carls ,@ls))
        (dlet* (((tval . rest) (mmember? x ls)))
          `(,tval . (,carls ,@rest)))))))

(define (freelistvars xl bvars fvars)
  (if (null? xl)
    (begin
      (assert (null? xl))
      `(,fvars ,@bvars))
    (dlet* (((x . xl) xl)
            ((nfvars . nbvars) (freelistvars xl bvars fvars)))
      (freevars x nbvars nfvars))))

(define (freevars x bvars fvars)
  "return new fvars and new bvars"
  (mcond
    ((msymbol? x)
     (dlet* (((x1 . x2) (mcopy x))
             ((x2 . x3) (mcopy x2))
             ((p1val . nbvars) (mmember? x1 bvars))
             ((p2val . nfvars) (mmember? x2 fvars)))
       (mif p1val
         (begin
           (free x3)
           `(,nfvars ,@nbvars)i)
         (mif p2val
           (begin
             (free x3)
             `(,nfvars ,@nbvars))
           `((,x ,@nfvars) ,@nbvars)))))
    ((matom? x)
     (free x)
     `(,fvars ,@bvars))
    (else
     (dlet* (((fn . args) x))
       (mcond
         ((meq? fn 'function)
          (free fn)
          (dlet* ((((lambda bvlist . body)) args))
            (free lambda)
            (freelistvars body `(,@bvlist ,@bvars) fvars)))
         (else
          (freelistvars `(,fn ,@args) bvars fvars)))))))
