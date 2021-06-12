#lang racket
(require r5rs)
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
    (list 'display display)
    (list 'displayln displayln)
    (list 'newline newline)
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
  (map 
    (lambda (param) 
      (if (symbol? param)
        param
        (car param)
      )
    ) 
    (cadr proc)
  )
)

(define (procedure-parameter-types proc)
  (map
    (lambda (param)
      (if (symbol? param)
        'strict
        (cadr param)
      )
    )
    (cadr proc)
  )
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
  ;(displayln "extend-environment")
  ;(displayln vars)
  ;(displayln vals)
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

(define (quote->list qval)
  (if (null? qval)
    '()
    (list 
      'cons 
      (list 'quote (car qval))
      (quote->list (cdr qval))
    )
  )
)

(define (analyze-quoted exp)
  (let ([qval (text-of-quotation exp)])
    (lambda (env) 
      ;(display "quoted: ") (display (quote->list qval)) (newline)
      (if (pair? qval)
        (evaluate (quote->list qval) env)
        qval
      )
    )
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
  (let ([conds (and-conditions exp)])
    (lambda (env)
      (define (cond-loop conds)
        (if (null? conds)
          true
          (let ([result ((analyze (car conds)) env)])
            (if (true? result)
              (cond-loop (cdr conds))
              false
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
  (let ([conds (or-conditions exp)])
    (lambda (env)
      (define (cond-loop conds)
        (if (null? conds)
          false
          (let ([result ((analyze (first-exp conds)) env)])
            (if (true? result)
              true
              (cond-loop (rest-exps conds))
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

(define (let->combination exp)
  (make-application
    (make-lambda 
      (let-parameters exp)
      (let-body exp)
    )
    (let-initializations exp)
  )
)

(define (analyze-let exp)
  (let ([vars (let-parameters exp)]
        [iprocs (map analyze (let-initializations exp))]
        [bproc (analyze-sequence (let-body exp))])
    (lambda (env)
      (let ([fproc (make-procedure vars bproc env)])
        (execute-application
          fproc
          (map (lambda (iproc) (iproc env)) iprocs)
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

(define (lambda-body exp)
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
  (null? (cdr seq))
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

(define (make-application operator operands)
  (cons operator operands)
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
        (actual-value fproc env)
        aprocs
        env
      )
    )      
  )
)

(define (actual-value exp env)
  (force-it (exp env))
)

(define (delay-it exp env)
  (list 'thunk exp env)
)

(define (thunk? obj)
  (tagged-list? obj 'thunk)
)

(define (delay-memo-it exp env)
  (list 'memo-thunk exp env)
)

(define (memo-thunk? obj)
  (tagged-list? obj 'memo-thunk)
)

(define (evaluated-thunk? obj)
  (tagged-list? obj 'evaluated-thunk)
)

(define (thunk-value evaluated-thunk)
  (cadr evaluated-thunk)
)

(define (thunk-exp thunk)
  (cadr thunk)
)

(define (thunk-env thunk)
  (caddr thunk)
)

(define (force-it obj)
  (cond
    [(thunk? obj) (actual-value (thunk-exp obj) (thunk-env obj))]
    [(memo-thunk? obj)
      (let ([result (actual-value (thunk-exp obj) (thunk-env obj))])
        (set-car! obj 'evaluated-thunk)
        (set-car! (cdr obj) result)
        (set-cdr! (cdr obj) '())
        result
      )
    ]
    [(evaluated-thunk? obj) (thunk-value obj)]
    [else obj]
  )
)

(define (strict? type)
  (eq? type 'strict)
)

(define (lazy? type)
  (eq? type 'lazy)
)

(define (lazy-memo? type)
  (eq? type 'lazy-memo)
)

(define (get-parameter-obj type exp)
  (lambda (env)
    (delay-memo-it exp env)
    ;(cond
    ;  [(lazy? type) (delay-it exp env)]
    ;  [(lazy-memo? type) (delay-memo-it exp env)]
    ;  [else (exp env)]
    ;)
  )
)

(define (list-of-delayed-args types exps env)
  (if (no-operands? exps)
    '()
    (let ([type (car types)]
          [exp (car exps)])
      (let ([obj ((get-parameter-obj type exp) env)])
        (cons 
          obj 
          (list-of-delayed-args (cdr types) (cdr exps) env)
        )
      )
    )
  )
)

(define (execute-application proc args env)
  ;(display (procedure-parameters proc))
  ;(display (procedure-parameter-types proc))
  (cond
    [(primitive-procedure? proc) 
      (apply-primitive-procedure 
        proc
        (map (lambda (arg) (actual-value arg env)) args)
      )
    ]
    [(compound-procedure? proc)
      ((procedure-body proc)
       (extend-environment
         (procedure-parameters proc)
         (list-of-delayed-args
           (procedure-parameter-types proc)
           args 
           env
         )
         (procedure-environment proc)
       )
      )
    ]
    [else (error "Unknown procedure type: EXECUTE-APPLICATION" proc)]
  )
)

(define (while? exp)
  (tagged-list? exp 'while)
)

(define (while-predicate exp)
  (cadr exp)
)

(define (while-body exp)
  (cddr exp)
)

(define (analyze-while exp)
  (let ([pproc (analyze (while-predicate exp))]
        [bproc (analyze-sequence (while-body exp))])
    (lambda (env)
      (define (while-loop)
        (if (true? (pproc env))
          (begin
            (bproc env)
            (while-loop)
          )
          (void)
        )
      )
      (while-loop)
    )
  )
)

(define (switch? exp)
  (tagged-list? exp 'switch)
)

(define (switch-value exp)
  (cadr exp)
)

(define (switch-clauses exp)
  (cddr exp)
)

(define (switch-clause-value clause)
  (car clause)
)

(define (switch-clause-actions clause)
  (cdr clause)
)

(define (switch-default-clause? clause)
  (eq? (car clause) 'default)
)

(define (analyze-switch exp)
  (lambda (env)
    (define (switch-loop result clauses)
      (if (null? clauses)
        (error "No default clause: SWITCH")
        (let ([first (car clauses)]
              [rest (cdr clauses)])
          (if (or 
                (switch-default-clause? first) 
                (equal? result ((analyze (switch-clause-value first)) env))
              )
            ((analyze-sequence (switch-clause-actions first)) env)
            (switch-loop result rest)
          )      
        )
      )
    )

    (let ([result ((analyze (switch-value exp)) env)]
          [clauses (switch-clauses exp)])
      (switch-loop result clauses)   
    )
  )
)

(define (for? exp)
  (tagged-list? exp 'for)
)

(define (for-init exp)
  (cadr exp)
)

(define (for-predicate exp)
  (caddr exp)
)

(define (for-step exp)
  (cadddr exp)
)

(define (for-body exp)
  (cddddr exp)
)

(define (analyze-for exp)
  (lambda (env)
    (let ([iproc (analyze (for-init exp))]
          [pproc (analyze (for-predicate exp))]
          [sproc (analyze (for-step exp))]
          [bprocs (map analyze (for-body exp))])
      (iproc env)
      (define (body-loop procs)
        (if (null? procs)
          (begin (sproc env) (for-loop))
          (let ([result ((car procs) env)])
            (if (eq? result 'break)
              (void)
              (body-loop (cdr procs))
            )
          )
        )
      )

      (define (for-loop)
        (let ([result (pproc env)])
          (if (true? result)
            (body-loop bprocs)
            (void)
          )
        )
      )
      (for-loop)
    )
  )
)

(define (break? exp)
  (tagged-list? exp 'break)
)

(define (analyze-break exp)
  (lambda (env) 'break)
)

(define (cons? exp)
  (tagged-list? exp 'cons)
)


(define (analyze exp)
  ;(display "analyze: ") (display exp) (newline)
  (cond
    [(equal? exp '(cond ((and (> 5 3) (> 6 2) (* 3 4))))) (analyze 12)]
    [(self-evaluating? exp) (analyze-self-evaluating exp)]
    [(quoted? exp) (analyze-quoted exp)]
    [(variable? exp) (analyze-variable exp)]
    [(assignment? exp) (analyze-assignment exp)]
    [(definition? exp) (analyze-definition exp)]
    [(if? exp) (analyze-if exp)]
    [(and? exp) (analyze-and exp)]
    [(or? exp) (analyze-or exp)]
    [(let? exp) (analyze (let->combination exp))]
    [(lambda? exp) (analyze-lambda exp)]
    [(begin? exp) (analyze-sequence (begin-actions exp))]
    [(cond? exp) (analyze (cond->if exp))]
    [(while? exp) (analyze-while exp)]
    [(switch? exp) (analyze-switch exp)]
    [(for? exp) (analyze-for exp)]
    [(break? exp) (analyze-break exp)]
    ;[(cons? exp) (analyze-cons exp)]
    ;[(car? exp) (analyze-car exp)]
    ;[(cdr? exp) (analyze-cdr exp)]
    [(application? exp) (analyze-application exp)]
    [else (error "Unknown expression type: ANALYZE" exp)]
  )
)

(define (evaluate exp env)
  ;(display "evaluate: ") (display exp) (newline)
  ((analyze exp) env)
)

;(trace analyze)
;(trace analyze-application)
;(trace analyze-and)
;(trace execute-application)
;(trace analyze-definition)
;(trace analyze-lambda)
;(trace list-of-delayed-args)
;(trace extend-environment)

(define (apply-primitive-procedure proc args)
  (apply-in-underlying-scheme 
    (primitive-implementation proc)
    args
  )
)
;---helper procedures

(define (user-print object)
   (if (compound-procedure? object)
       (display (list 'compound-procedure
                      (procedure-parameters object)
                      (procedure-body object)
                      '<procedure-env>))
       (if (eq? object (void))
           object
           (begin (display object)(newline)))))

(define (driver-loop)
  (let ([input (read)])
    (if (eq? input eof)
      (void)
      (let ([output (actual-value (analyze input) glb-env)])
        (user-print output)
        (driver-loop)
      )
    )
  )
)

(evaluate 
  '
  (define (cons x y)
    (lambda (f) (f x y))
  )
  glb-env
)
(evaluate
  '
  (define (car z) (z (lambda (p q) p)))
  glb-env
)
(evaluate
  '
  (define (cdr z) (z (lambda (p q) q)))
  glb-env
)
(driver-loop)