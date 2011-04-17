

;; Ex. 4.1
(define (list-of-values exps env)
  (if (no-operands? exps)
      '()
      (let* ((first (eval (first-operand exp) env))
             (rest (list-of-values (rest-operands exps) env)))
        (cons first rest))))

;; Ex. 4.3
(define (eval exp env)
  (cond ((self-evaluating? exp) exp)
        ((variable? exp) exp)
        ((get-eval (car exp)) =>
         (lambda (proc) (proc exp env)))
        ((application? exp)
         (apply (eval (operator exp) env)
                (list-of-values (operands exp) env)))
        (else
         (error "Unknown expression type -- EVAL" exp))))

(define eval-procs '())

(define (get-eval tag)
  (let ((pair (assoc tag eval-procs)))
    (and pair (cdr pair))))

(define (put-eval tag proc)
  (let ((pair (assoc tag eval-procs)))
    (if pair
        (set-cdr! pair proc)
        (set! eval-procs
              (cons (cons tag proc)
                    eval-procs)))))

(put-eval 'quote
          (lambda (exp env) (cadr exp)))
(put-eval 'set! eval-assignment)
(put-eval 'define eval-definition)
(put-eval 'if eval-if)
(put-eval 'lambda
          (lambda (exp env)
            (make-procedure (lambda-parameters exp)
                            (lambda-body exp)
                            env)))
(put-eval 'begin
          (lambda (exp env)
            (eval-sequence (begin-actions exp) env)))
(put-eval 'cond
          (lambda (exp env)
            (eval (cond->if exp) env)))

;; Ex. 4.4
(define true #t)
(define false #f)

(define (eval-or exp env)
  (define (or* clauses)
    (if (null? clauses)
        false
        (let* ((clause (car clauses))
               (value (eval clause env)))
          (if (true? value)
              value
              (or* (cdr clauses) env)))))
  (or* (cdr exp) env))

(define (eval-and exp env)
  (define (and* clauses)
    (let* ((clause (car clauses))
           (value (eval clause env)))
      (if (true? value)
          (if (null? (cdr clauses))
              value
              (and* (cdr clauses) env))
          false)))
  (if (null? (cdr exp))
      true
      (and* (cdr exp))))

(put-eval 'or eval-or)
(put-eval 'and eval-and)

;; Ex. 4.5
(define (last-exp? seq) (null? (cdr seq)))

(define (first-exp seq) (car seq))

(define (sequence->exp seq)
  (cond ((null? seq) seq)
        ((last-exp? seq) (first-exp seq))
        (else (make-begin seq))))

(define (make-begin seq) (cons 'begin seq))

(define (make-definition symbol value)
  (list 'define symbol value))

(define (make-application proc . params)
  (cons proc params))

(define (make-if predicate consequent alternative)
  (list 'if predicate consequent alternative))

(define (expand-clauses clauses)
  (if (null? clauses)
      'false
      (let ((first (car clauses))
            (rest (cdr clauses)))
        (if (cond-else-clause? first)
            (if (null? rest)
                (sequence->exp (cond-actions first))
                (error "ELSE clause isn't last -- COND->IF"
                       clauses))
            (let ((actions (cond-actions first))
                  (predicate (cond-predicate first)))
              (if (eq? '=> (car actions))
                  (if (= 2 (length actions))
                      (let ((sym (gensym)))
                        (make-if (sequence->exp
                                  (list
                                   (make-definition sym predicate)
                                   sym))
                                 (make-application (cadr actions)
                                                   sym)
                                 (expand-clauses rest)))
                      (error "COND clause with '=> must have single consequent expression -- CONT->IF"
                             actions))
                  (make-if predicate
                           (sequence->exp actions)
                           (expand-clauses rest))))))))

;; Ex. 4.6
(define (make-lambda parameters body)
  (cons 'lambda (cons parameters body)))

(define let-bindings cadr)

(define let-body cddr)

(define (let->combination exp)
  (let* ((bindings (let-bindings exp))
         (symbols (apply car bindings))
         (values (apply cadr bindings)))
    (make-application (make-lambda (list symbols) (let-body exp))
                      values)))

(put-eval 'let
          (lambda (exp env)
            (eval (let->combination exp) env)))

;; Ex. 4.7
(define (make-let bindings body)
  (cons 'let (cons bindings body)))

(define (let*->nested-lets exp)
  (define (convert bindings body)
    (if (null? (cdr bindings))
        (make-let bindings body)
        (make-let (list (car bindings))
                  (list (convert (cdr bindings)
                                 body)))))
  (convert (let-bindings exp) (let-body exp)))

(put-eval 'let*
          (lambda (exp env)
            (eval (let*->nested-lets exp) env)))

;; Ex. 4.8
(define (let-var? exp)
  (symbol? (cadr exp)))

(define (let-var exp)
  (if (let-var? exp)
      (cadr exp)
      false))

(define (let-bindings exp)
  (if (let-var? exp)
      (caddr exp)
      (cadr exp)))

(define (let-body exp)
  (if (let-var? exp)
      (cdddr exp)
      (cddr exp)))

(define (let->combination exp)
  (let* ((bindings (let-bindings exp))
         (symbols (apply car bindings))
         (values (apply cadr bindings))
         (proc (make-lambda (list symbols) (let-body exp))))
    (if (let-var? exp)
        (let ((var (let-var exp)))
          (sequence->exp (list (make-definition var proc)
                               (make-application var values))))
        (make-application proc
                          values))))

;; Ex. 4.9
(define while-predicate cadr)

(define while-body cddr)

(define (while->combination exp)
  (let ((predicate (while-predicate exp))
        (body (while-body exp))
        (var (gensym)))
    (sequence->exp
     (list
      (make-definition
       var
       (make-lambda '()
               (make-if predicate
                        (append
                         body
                         (make-application var))
                        ''done)))
      (make-application var)))))

(put-eval 'while
          (lambda (exp env)
            (eval (while->combination exp) env)))

;; Ex. 4.11
(define (make-frame variables values)
  (cons 'bindings
        (map cons variables values)))

(define (frame-variables frame)
  (map car (cdr frame)))

(define (frame-values frame)
  (map cdr (cdr frame)))

(define (add-binding-to-frame! var val frame)
  (set-cdr! frame (cons (cons var val) (cdr frame))))

;; Ex. 4.12
(define (variable-cell var env)
  (define (env-loop env)
    (if (eq? env the-empty-environment)
        #f
        (or (assoc (cdr (first-frame env)))
            (env-loop (enclosing-environment env))))))

(define (set-variable-value! var val env)
  (let ((cell (variable-cell var env)))
    (if cell
        (set-cdr! cell val)
        (error "Unbound variable -- SET!" var))))

(define (lookup-variable-value var env)
  (let ((cell (variable-cell var env)))
    (if cell
        (cdr cell)
        (error "Unbound variable" var))))

(define (define-variable! var val env)
  (let* ((frame (first-frame env))
         (cell (assoc (cdr frame))))
    (if cell
        (set-cdr! cell val)
        (add-binding-to-frame! var val frame))))

;; Ex. 4.16
(define (lookup-variable-value var env)
  (let ((cell (variable-cell var env)))
    (if (or (not cell) (eq? (cdr cell) '*unassigned*))
        (error "Unbound variable" var))))

(define (scan-out-defines body)
  (define (scan defns body)
    (if (and (pair? body)
             (definition? (car body)))
        (scan (cons (car body) defns)
              (cdr body))
        (cons (map cdr defns) body)))
  (let* ((scanned (scan '() body))
         (definitions (car scanned))
         (vars (map car definitions))
         (vals (map cdr definitions))
         (body (cdr scanned)))
    (make-let (map (lambda (var) (list var ''*unassigned*))
                   vars)
              (append (map (lambda (var val) (list 'set! var val))
                           vars vals)
                      body))))

(define (make-procedure parameters body env)
  (list 'procedure parameters
        ;; that way the body is only processed once
        (scan-out-defines body) env))

;; Ex. 4.21
(define (f x)
  ((lambda (even? odd?)
     (even? even? odd? x))
   (lambda (ev? od? n)
     (if (= n 0) true (od? ev? od? (1- n))))
   (lambda (ev? od? n)
     (if (= n 0) false (ev? ev? od? (1- n))))))

;; Ex. 4.22
(define (analyze-let exp)
  (analyze (let->combination exp)))
