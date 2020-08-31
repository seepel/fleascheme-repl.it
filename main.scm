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

;; Core polyfill
(define modulo mod)

;; srfi-1
(define (car+cdr pair)
  (values (car pair) (cdr pair)))

(define first car)
(define second cadr)
(define third caddr)
(define fourth cadddr)

(define *error* #f)

(define (error message . args)
  (let ((output (open-output-string)))
    (display message output)
    (for-each (lambda (x) (write x output))
	            args)
    (if *error*
        (*error* (get-output-string output))
        (raise (get-output-string output)))))

(define lambda-parameters cadr)
(define lambda-body cddr)

(define (bound-identifier? identifier bound-identifiers)
  (cond 
    ((null? bound-identifiers)
     (values #f bound-identifiers))
    ((eq? identifier (car bound-identifiers))
     (values #t (cdr bound-identifiers)))
    (else
      (let-values (((true? remaining-bound-identifiers)
                    (bound-identifier? identifier (cdr bound-identifiers)))) 
       (values true?
               (cons (car bound-identifiers)
                     remaining-bound-identifiers))))))
                     
(define (free-variables expression bound-identifiers environment)
  (cond
    ((self-quoting? expression)
     (values '() bound-identifiers environment))
    ((identifier? expression)
     (let-values (((bound? bound-identifiers)
                   (bound-identifier? expression bound-identifiers)))
       (if bound?
           (values '() bound-identifiers environment)
           (let-values (((binding environment) (environment-ref environment expression)))
             (if binding
                 (values (list binding) bound-identifiers environment)
                 (error "[free-variables identifier?] Unbound identifier: " expression))))))
    ((null? expression)
     (values '() bound-identifiers environment))
    ((pair? expression)
     (case (first expression)
       ((quote)
        (values '() bound-identifiers environment))
       ((lambda)
        (free-variables (lambda-body expression) 
                        (append (lambda-parameters expression) bound-identifiers)
                        environment))
       ((begin if)
        (let*-values (((test-free-variables test-bound-identifiers test-environment)
                       (free-variables (second expression) 
                                       bound-identifiers 
                                       environment))
                      ((true-free-variables true-bound-identifiers true-environment)
                       (free-variables (third expression) 
                                       test-bound-identifiers 
                                       test-environment))
                      ((false-free-variables false-bound-identifiers false-environment)
                       (free-variables (third expression) 
                                       test-bound-identifiers 
                                       test-environment)))
          (if (and (equal? true-free-variables false-free-variables)
                   (equal? true-bound-identifiers false-bound-identifiers)
                   (equal? true-environment false-environment))
              (values (append test-free-variables true-free-variables)
                      true-bound-identifiers
                      true-environment)
              (error "[free-variables if] References must match between conditional branches"))))
       ((dup)
        (let ((free-variable? (assv (second expression) environment))
              (bound-variable? (find (lambda (x) 
                                       (eqv? (second expression) x))
                                     bound-identifiers)))
          (cond
            (free-variable? (values (list (second binding)) bound-identifiers environment))
            (bound-variable? (values '() bound-identifiers environment))
            (error "[free-variables dup] Unbound identifier: " (second expression)))))
       (else 
        (let*-values (((a d)
                            (car+cdr expression))
                      ((a-free-variables bound-identifiers environment)
                       (free-variables a bound-identifiers environment))
                      ((d-free-variables bound-identifiers environment) 
                       (free-variables d bound-identifiers environment)))
          (values (append a-free-variables d-free-variables) bound-identifiers environment)))))))

(define (primitive-procedure? x)
  (procedure? x))

(define (compound-procedure? x)
  (eqv? (car x) 'compound-procedure))

(define (make-procedure parameters body environment)
  (list 'compound-procedure parameters body environment))

(define (feval-lambda parameters body environment)
  (let*-values (((procedure-environment _ environment) (free-variables body parameters environment)))
    (values (make-procedure parameters body procedure-environment) environment)))

(define (procedure-parameters procedure)
  (cadr procedure))

(define (procedure-body procedure)
  (caddr procedure))

(define (procedure-environment procedure)
  (cadddr procedure))

(define (acons key value alist)
  (cons (cons key value) alist))

(define empty-environment 
  `((+ . ,+) (- . ,-) (* . ,*) (/ . ,/) (modulo . ,modulo)
    (cons . ,cons) (car . ,car) (cdr . ,cdr) (car+cdr . ,car+cdr)
    (> . ,>) (< . ,<) (>= . ,>=) (<= . ,<=) (= . ,=)
    (eq? . ,eq?) (eqv? . ,eqv?) (equal? . ,equal?)
    (display . ,display) (write . ,write) (newline . ,newline)
    (values . ,values)
    ))

(define (extend-environment environment bindings values)
  (cond 
    ((null? bindings)
     environment)
    ((null? values)
     (extend-environment (acons (car bindings) #f environment)
                         (cdr bindings)
                         values))
    (else
      (extend-environment (acons (car bindings) (car values) environment)
                          (cdr bindings)
                          (cdr values)))))

(define (environment-ref environment identifier)
  (if (null? environment)
      (values #f environment)
      (let-values (((binding environment) (car+cdr environment)))
        (if (eqv? (car binding) identifier)
            (if (primitive-procedure? (cdr binding))
              (values binding (cons binding environment))
              (values binding environment))
            (let-values (((found-binding environment) 
                          (environment-ref environment identifier)))
              (values found-binding (cons binding environment)))))))

(define (self-quoting? x)
  (or (number? x)
      (boolean? x)
      (string? x))) 

(define identifier? symbol?)

(define (feval-identifier identifier environment)
  (let-values (((binding environment) (environment-ref environment identifier)))
    (if binding
        (values (cdr binding) environment)
        (error "[feval-identifier] Unbound identifier: " identifier))))

(define application? pair?)

(define (feval-operands operands environment)
  (if (null? operands)
      (values operands environment)
      (let*-values (((operand environment) (feval (car operands) environment))
                    ((rest environment) (feval-operands (cdr operands) environment)))
        (values (cons operand rest) environment))))

(define (feval-application expression environment)
  (let*-values (((operator environment)
                 (feval (car expression) environment))
                ((operands environment)
                 (feval-operands (cdr expression) environment)))
    (values (fapply operator operands) environment)))

(define unspecified '*unspecified*)

(define (feval-sequence expressions environment)
  (cond
    ((null? expressions) 'unspecified)
    ((null? (cdr expressions)) (feval (car expressions) environment))
    (else (let-values (((value environment)
                        (feval (car expressions) environment)))
            (feval-sequence (cdr expressions) environment)))))

(define (feval-define expression environment)
  (if (pair? (cadr expression))
      (feval `(define ,(caadr expression)
                (lambda ,(cdadr expression)
                  ,@(cddr expression)))
             environment)
      (let*-values (((identifier) (cadr expression))
                    ((self-binding) (cons identifier '*self*))
                    ((environment) (if (and (pair? (caddr expression))
                                            (eqv? (caaddr expression) 'lambda))
                         (cons self-binding environment)
                         environment))
                    ((value environment) (feval (caddr expression) environment)))
        (when (and (pair? environment)
                   (eqv? self-binding (car environment)))
          (set! environment (cdr environment)))
        (values unspecified (acons identifier value environment)))))

(define (feval-dup identifier environment)
  (if (symbol? identifier)
      (let ((binding (assv identifier environment)))
        (if binding
            (values (cdr binding) environment)
            (error "Error: dup: Unobound identifier: " identifier)))
      (error "Error: dup: Expected an identifier but got: " identifier)))

(define (feval expression environment)
  (cond
    ((self-quoting? expression)
     (values expression environment))
    ((identifier? expression)
     (feval-identifier expression environment))
    ((application? expression)
     (case (car expression)
       ((dup)
        (feval-dup (second expression) environment))
       ((lambda)
        (feval-lambda (cadr expression) (cddr expression) environment))
       ((quote) 
        (values (cadr expression) environment))
       ((define)
        (feval-define expression environment))
       ((begin)
        (feval-sequence (cdr expression) environment))
       ((if)
        (let*-values (((true? environment) (feval (second expression) environment)))
          (if true?
              (feval (third expression) environment)
              (feval (fourth expression) environment))))
       ((exit)
        (values expression environment))
       (else (feval-application expression environment))))
    (else (error "Cannot evaluate: " expression))))

(define (apply-primitive-procedure procedure arguments)
  (apply procedure arguments))

(define (unused-bindings environment)
  (if (null? environment)
      '()
      (dlet* ((((identifier . value) . environment) environment))
        (if (primitive-procedure? value)
            (unused-bindings environment)
            (acons identifier value (unused-bindings environment))))))

(define (fapply operator operands)
  (cond 
    ((primitive-procedure? operator)
     (apply-primitive-procedure operator operands))
    ((compound-procedure? operator)
     (let*-values (((environment)
                    (map (lambda (binding)
                           (if (eqv? (cdr binding) '*self*)
                             (cons (car binding) operator)
                             binding))
                         (extend-environment (procedure-environment operator)
                                             (procedure-parameters operator)
                                                           operands)))
                   ((value environment) (feval-sequence (procedure-body operator) environment))
                   ((unused-bindings) (unused-bindings environment)))
       (if (null? unused-bindings) 
           value
           (error "Unused bindings:" unused-bindings))))
    (else (error "Wrong type to apply: ~a" operator))))

(define (repl)
  (%repl empty-environment))    

(define (%repl environment)
  (display "* ")
  (call-with-current-continuation
   (lambda (cont)
     (set! *error* 
       (lambda (error)
         (display error)
         (newline)
         (cont (%repl environment))))
     (let*-values (((input) (read))
                   ((output environment)  (feval input environment)))
       (display "=> ")
       (write output)
       (newline)
       (if (and (pair? input)
                    (eqv? (car input) 'exit))
           (set! *error* #f)
           (%repl environment))))))

(define fizzbuzz
  '(define (fizzbuzz x y)
     (display
       (if (= (modulo (dup x) 15) 0)
           "FizzBuzz"
           (if (= (modulo (dup x) 3) 0)
             "Fizz"
             (if (= (modulo (dup x) 5) 0)
               "Buzz"
               (dup x)))))
     (newline)
     (if (< (dup x) (dup y))
         (fizzbuzz (+ x 1) y)
         (begin x y fizzbuzz))))
