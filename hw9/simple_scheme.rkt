#lang racket
(require r5rs)
(require racket/trace)


;---global define
(define apply-in-underlying-scheme apply)

(define (tagged-list? exp tag)
  (if (pair? exp)
    (eq? (car exp) tag)
    false
  )
)

(define (true? x)
  (not (eq? x false))
)

(define (false? x)
  (eq? x false)
)
;---procedures

(define primitive-procedures
  (list
    (list 'car car)
    (list 'cdr cdr)
    (list 'cons cons)
    (list 'cadr cadr)
    (list 'cddr cddr)
    (list 'caddr caddr)
    (list 'cdddr cdddr)
    (list 'cadddr cadddr)
    (list 'cddddr cddddr)
    (list 'null? null?)
    (list '+ +)
    (list '- -)
    (list '* *)
    (list '/ /)
    (list '> >)
    (list '>= >=)
    (list '< <)
    (list '<= <=)
    (list '= =)
    (list 'eq? eq?)
    (list 'equal? equal?)
    (list 'symbol? symbol?)
    (list 'number? number?)
    (list 'pair? pair?)
    (list 'sqrt sqrt)
    (list 'remainder remainder)
    (list 'not not)
    (list 'length length)
    (list 'append append)
    (list 'map map)
    (list 'filter filter)
    (list 'list list) 
  )
)

(define (primitive-procedures-names)
  (map car primitive-procedures)
)

(define (primitive-procedures-objects)
  (map (lambda (proc) (list 'primitive (cadr proc))) primitive-procedures)
)

(define (primitive-procedure? proc)
  (tagged-list? proc 'primitive)
)

(define (primitive-implementation proc)
  (cadr proc)
)

(define (compound-procedure? proc)
  (tagged-list? proc 'procedure)
)

(define (procedure-parameters proc)
  (cadr proc)
)

(define (procedure-body proc)
  (caddr proc)
)

(define (procedure-environment proc)
  (cadddr proc)
)

(define (make-procedure parameters body env)
  (list 'procedure parameters body env)
)

;---frame procedures

(define (make-frame variables values)
  (cons variables values)
)

(define (frame-variables frame)
  (car frame)
)

(define (frame-values frame)
  (cdr frame)
)

(define (add-binding-to-frame! var val frame)
  (set-car! frame (cons var (car frame)))
  (set-cdr! frame (cons val (cdr frame)))
)

;---environment procedures

(define (enclosing-environment env)
  (cdr env)
)

(define (first-frame env)
  (car env)
)

(define (the-empty-environment)
  '()
)

(define (extend-environment vars vals base-env)
  (if (= (length vars) (length vals))
    (cons (make-frame vars vals) base-env)
    (if (< (length vars) (length vals))
      (error "Too many arguments supplied" vars vals)
      (error "Too few arguments supplied" vars vals)
    )
  )
)

(define (define-variable! var val env)
  (let ([frame (first-frame env)])
    (define (scan vars vals)
      (cond [(null? vars) (add-binding-to-frame! var val frame)]
            [(eq? var (car vars)) (set-car! vals val)]
            [else (scan (cdr vars) (cdr vals))]
      )
    )

    (scan
      (frame-variables frame)
      (frame-values frame)
    )
  )
)

(define (set-variable-value! var val env)
  (define (env-loop env)
    (define (scan vars vals)
      (cond [(null? vars) (env-loop (enclosing-environment env))]
            [(eq? var (car vars)) (set-car! vals val)]
            [else (scan (cdr vars) (cdr vals))]
      )
    )

    (if (eq? env the-empty-environment)
      (error "Unbound variable -- SET!" var)
      (let ([frame (first-frame env)])
        (scan
          (frame-variables frame)
          (frame-values frame)
        )
      )
    )
  )

  (env-loop env)
)

(define (lookup-variable-value var env)
  (define (env-loop env)
    (define (scan vars vals)
      (cond [(null? vars) (env-loop (enclosing-environment env))]
            [(eq? var (car vars)) (car vals)]
            [else (scan (cdr vars) (cdr vals))]
      )
    )

    (if (eq? env the-empty-environment)
      (error "Unbound variable" var)
      (let ([frame (first-frame env)])
        (scan
          (frame-variables frame)
          (frame-values frame)
        )
      )
    )
  )

  (env-loop env)
)

(define (setup-environment)
  (let ([initial-env 
          (extend-environment
            (primitive-procedures-names)
            (primitive-procedures-objects)
            the-empty-environment)
        ])
    (define-variable! 'true true initial-env)
    (define-variable! 'false false initial-env)
    initial-env
  )
)

(define glb-env (setup-environment))

;---eval procedures

(define (self-evaluating? exp)
  (cond
    [(number? exp) true]
    [(string? exp) true]
    [(eq? exp '()) true]
    [else false]
  )
)

(define (analyze-self-evaluating exp)
  (lambda (env) exp)
)

(define (quoted? exp)
  (tagged-list? exp 'quote)  
)

(define (text-of-quotation exp)
  (cadr exp)
)

(define (analyze-quoted exp)
  (let ([qval (text-of-quotation exp)])
    (lambda (env) qval)
  )
)

(define (variable? exp)
  (symbol? exp)
)

(define (analyze-variable exp)
  (lambda (env)
    (lookup-variable-value exp env)
  )
)

(define (assignment? exp)
  (tagged-list? exp 'set!)
)

(define (assignment-variable exp)
  (cadr exp)
)

(define (assignment-value exp)
  (caddr exp)
)

(define (analyze-assignment exp)
  (let ([var (assignment-variable exp)]
        [vproc (analyze (assignment-value exp))])
    (lambda (env)
      (set-variable-value! var (vproc env) env) 
      (void)
    )
  ) 
)

(define (definition? exp)
  (tagged-list? exp 'define)
)

(define (definition-variable exp)
  (if (symbol? (cadr exp))
    (cadr exp)
    (caadr exp)
  )
)

(define (definition-value exp)
  (if (symbol? (cadr exp))
    (caddr exp)
    (make-lambda (cdadr exp) (cddr exp))
  )
)

(define (make-variable-definition variable value)
  (list 'define variable value)
)

(define (analyze-definition exp)
  (let ([var (definition-variable exp)]
        [vproc (analyze (definition-value exp))])
    (lambda (env)
      (define-variable! var (vproc env) env)
      (void)
    )
  )
)

(define (if? exp)
  (tagged-list? exp 'if)
)

(define (if-predicate exp)
  (cadr exp)
)

(define (if-consequent exp)
  (caddr exp)
)

(define (if-alternative exp)
  (if (not (null? (cadddr exp)))
    (cadddr exp)
    'false
  )
)

(define (make-if predicate consequent alternative)
  (list 'if predicate consequent alternative)
)

(define (analyze-if exp)
  (let ([pproc (analyze (if-predicate exp))]
        [cproc (analyze (if-consequent exp))]
        [aproc (analyze (if-alternative exp))])
    (lambda (env)
      (if (true? (pproc env))
        (cproc env)
        (aproc env)
      )
    )      
  )
)

(define (and? exp)
  (tagged-list? exp 'and)
)

(define (and-conditions exp)
  (cdr exp)
)

(define (analyze-and exp)
  (let ([conds (map analyze (and-conditions exp))])
    (lambda (env)
      (define (cond-loop conds)
        (if (null? conds)
          'true
          (let ([result ((analyze (car conds)) env)])
            (if (true? result)
              (cond-loop (cdr conds))
              'false
            )
          )
        )
      )
      (cond-loop conds)
    )
  )
)

(define (or? exp)
  (tagged-list? exp 'or)
)

(define (or-conditions exp)
  (cdr exp)
)

(define (analyze-or exp)
  (let ([conds (map analyze (or-conditions exp))])
    (lambda (env)
      (define (cond-loop conds)
        (if (null? conds)
          'false
          (let ([result ((analyze (car conds)) env)])
            (if (true? result)
              'true
              (cond-loop (cdr conds))
            )
          )
        )
      )
      (cond-loop conds)
    )
  )
)

(define (let? exp)
  (tagged-list? exp 'let)
)

(define (let-definitions exp)
  (cadr exp)
)

(define (let-parameters exp)
  (map car (let-definitions exp))
)

(define (let-initializations exp)
  (map cadr (let-definitions exp))
)

(define (let-body exp)
  (cddr exp)
)

(define (analyze-let exp)
  (let ([vars (let-parameters exp)]
        [iprocs (map analyze (let-initializations exp))]
        [bproc (analyze-sequence (let-body exp))])
    (lambda (env)
      (let ([fproc (make-procedure vars bproc env)])
        (execute-application
          (fproc env)
          (map (lambda (iproc) (iproc env) iprocs))
        )
      )
    )
  )
)

(define (lambda? exp)
  (tagged-list? exp 'lambda)
)

(define (lambda-parameters exp)
  (cadr exp)
)

(define (lambda-body)
  (cddr exp)
)

(define (make-lambda parameters body)
  (cons 'lambda (cons parameters body))
)

(define (analyze-lambda exp)
  (let ([vars (lambda-parameters exp)]
        [bproc (analyze-sequence (lambda-body exp))])
    (lambda (env)
      (make-procedure vars bproc env)
    )      
  )
)
(define (begin? exp)
  (tagged-list? exp 'begin)
)

(define (begin-actions exp)
  (cdr exp)
)

(define (last-exp? seq)
  (null? (cdr exp))
)

(define (first-exp seq)
  (car seq)
)

(define (rest-exps seq)
  (cdr seq)
)

(define (make-begin seq)
  (cons 'begin seq)
)

(define (sequence->exp seq)
  (cond
    [(null? seq) seq]
    [(last-exp? seq) (first-exp seq)]
    [else (make-begin seq)]
  )
)

(define (analyze-sequence exps)

  (define (sequentially proc1 proc2)
    (lambda (env)
      (proc1 env) (proc2 env)
    )
  )

  (define (loop first-proc rest-procs)
    (if (null? rest-procs)
      first-proc
      (loop
        (sequentially first-proc (car rest-procs))
        (cdr rest-procs) 
      )
    )
  )

  (let ([procs (map analyze exps)])
    (if (null? procs)
      (error "Empty sequence: ANALYZE")
      (loop (car procs) (cdr procs))
    )
  )
)

(define (cond? exp)
  (tagged-list? exp 'cond)
)

(define (cond-clauses exp)
  (cdr exp)
)

(define (cond-predicate clause)
  (car clause)
)

(define (cond-actions clause)
  (cdr clause)
)

(define (cond-else-clause? clause)
  (eq? (cond-predicate clause) 'else)
)

(define (expand-clauses clauses)
  (if (null? clauses)
    'false
    (let ([first (car clauses)]
          [rest (cdr clauses)])
      (if (cond-else-clause? first)
        (if (null? rest)
          (sequence->exp (cond-actions first))
          (error "ELSE clause isn't last -- COND->IF" clauses)
        )
        (make-if
          (cond-predicate first)
          (sequence->exp (cond-actions first))
          (expand-clauses rest)
        )
      )      
    ) 
  )
)

(define (cond->if exp)
  (expand-clauses (cond-clauses exp))
)

(define (application? exp)
  (pair? exp)
)

(define (operator exp)
  (car exp)
)

(define (operands exp)
  (cdr exp)
)

(define (no-operands? ops)
  (null? ops)
)

(define (first-operand ops)
  (car ops)
)

(define (rest-operands ops)
  (cdr ops)
)

(define (analyze-application exp)
  (let ([fproc (analyze (operator exp))]
        [aprocs (map analyze (operands exp))])
    (lambda (env)
      (execute-application
        (fproc env)
        (map (lambda (aproc) (aproc env)) aprocs)
      )
    )      
  )
)

(define (execute-application proc args)
  (cond
    [(primitive-procedure? proc) (apply-primitive-procedure proc args)]
    [(compound-procedure? proc)
      ((procedure-body proc)
       (extend-environment
         (procedure-parameters proc)
         args
         (procedure-environment proc)
       )
      )
    ]
    [else (error "Unknown procedure type: EXECUTE-APPLICATION" proc)]
  )
)

(define (analyze exp)
  (displayln exp)
  (cond
    [(self-evaluating? exp) (analyze-self-evaluating exp)]
    [(quoted? exp) (analyze-quoted exp)]
    [(variable? exp) (analyze-variable exp)]
    [(assignment? exp) (analyze-assignment exp)]
    [(definition? exp) (analyze-definition exp)]
    [(if? exp) (analyze-if exp)]
    [(and? exp) (analyze-and exp)]
    [(or? exp) (analyze-or exp)]
    [(let? exp) (analyze-let exp)]
    [(lambda? exp) (analyze-lambda exp)]
    [(begin? exp) (analyze-sequence (begin-actions exp))]
    [(cond? exp) (analyze (cond->if exp))]
    [(application? exp) (analyze-application exp)]
    [else (error "Unknown expression type: ANALYZE" exp)]
  )
)

(define (eval exp env)
  ((analyze exp) env)
)

(trace analyze)
(trace execute-application)

(define (apply-primitive-procedure proc args)
  (apply-in-underlying-scheme 
    (primitive-implementation proc)
    args
  )
)
;---helper procedures

(define (user-print object)
  (displayln object)
)

(define (driver-loop)
  (let ([input (read)])
    (if (eq? input eof)
      (void)
      (let ([output (eval input glb-env)])
        (user-print output)
        (driver-loop)
      )
    )
  )
)

(driver-loop)