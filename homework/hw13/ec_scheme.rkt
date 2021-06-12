#lang racket
(require racket/mpair)
;---starts here
(require r5rs)

(define (tagged-list? exp tag)
  (if (pair? exp)
    (eq? (car exp) tag)
    false
  )
)

(define apply-in-underlying-scheme apply)

(define (true? x)
  (not (eq? x false))
)

(define (false? x)
  (eq? x false)
)

(define (eof? x)
  (eq? x eof)
)

(define (user-print object)
   (if (compound-procedure? object)
       (display (list 'compound-procedure
                      (procedure-parameters object)
                      (procedure-body object)
                      '<procedure-env>))
       (if (eq? object 'ok)
          (void)
          (begin (display object)(newline)))))
;----------------------------- environment
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
    (list 'display display)
    (list 'displayln displayln)
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

(define (apply-primitive-procedure proc args)
  (apply-in-underlying-scheme 
    (primitive-implementation proc)
    args
  )  
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

(define (get-global-environment)
  glb-env
)
;----------------------------- evaluators
(define (self-evaluating? exp)
  (cond
    [(number? exp) true]
    [(string? exp) true]
    [(eq? exp '()) true]
    [else false]
  )
)

(define (quoted? exp)
  (tagged-list? exp 'quote)  
)

(define (text-of-quotation exp)
  (cadr exp)
)

(define (variable? exp)
  (symbol? exp)
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

(define (and? exp)
  (tagged-list? exp 'and)
)

(define (and-conditions exp)
  (cdr exp)
)

(define (or? exp)
  (tagged-list? exp 'or)
)

(define (or-conditions exp)
  (cdr exp)
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

(define (last-operand? ops)
  (null? (cdr ops))
)

(define (no-more-exps? exps)
  (null? exps)
)

(define (empty-arglist) '())

(define (adjoin-arg arg arglist) 
  (append arglist (list arg))
)
;----------------------------- register machine
(define (make-register name)
  (let ([contents '*unassigned*])
    (define (dispatch message)
      (cond
        [(eq? message 'get) contents]
        [(eq? message 'set) 
          (lambda (value)
            (set! contents value)
          )
        ] 
        [else (error "Unknown request: REGISTER" message)]
      )
    ) 
    dispatch
  ) 
)

(define (get-contents register)
  (register 'get)
)

(define (set-contents! register value)
  ((register 'set) value)
)

(define (make-stack)
  (let ([s '()])
    (define (push x) 
      (set! s (cons x s))
    )
    (define (pop) 
      (if (null? s)
        (error "Empty stack: POP")
        (let ([top (car s)])
          (set! s (cdr s))
          top
        )
      )
    )
    (define (initialize)
      (set! s '())
      (void)
    )
    (define (dispatch message)
      (cond
        [(eq? message 'push) push]
        [(eq? message 'pop) (pop)]
        [(eq? message 'initialize) (initialize)]
        [else (error "Unknown request: STACK" message)]
      )
    )
    dispatch
  )
)

(define (pop stack)
  (stack 'pop)
)

(define (push stack value)
  ((stack 'push) value)
)

(define (get-register machine register-name)
  ((machine 'get-register) register-name)
)

(define (get-register-contents machine register-name)
  (get-contents (get-register machine register-name))
)

(define (set-register-contents! machine register-name value)
  (set-contents! (get-register machine register-name) value)
)

(define (assemble controller-text machine)
  (extract-labels
    controller-text
    (lambda (insts labels)
      (update-insts! insts labels machine)
      insts
    )
  )
)

(define (extract-labels text receive)
  (if (null? text)
    (receive '() '())
    (extract-labels
      (cdr text)
      (lambda (insts labels)
        (let ([next-inst (car text)])
          (if (symbol? next-inst)
            (begin
              ;(display "label: ") (display next-inst) (newline)
              (receive insts (cons (make-label-entry next-inst insts) labels))
            )
            (begin 
              ;(display "instruction: ") (display next-inst) (newline)
              (receive (cons (make-instruction next-inst) insts) labels)
            )
          )
        )
      ) 
    )
  )
)

(define (update-insts! insts labels machine)
  (let ([pc (get-register machine 'pc)]
        [flag (get-register machine 'flag)]
        [stack (machine 'stack)]
        [ops (machine 'operations)])
    (for-each
      (lambda (inst)
        ;(display "update-insts! ") (display inst) (newline)
        (set-instruction-execution-proc!
          inst
          (make-execution-procedure
            (instruction-text inst)
            labels machine pc flag stack ops
          )
        )
      )
      insts
    ) 
  )
)

(define (make-instruction text)
  (cons text '())
)

(define (instruction-text inst)
  (car inst)
)

(define (instruction-execution-proc inst)
  (cdr inst)
)

(define (set-instruction-execution-proc! inst proc)
  ;(display "set-instruction-execution-proc!" ) (display inst) (display proc) (newline)
  (set-cdr! inst proc)
)

(define (make-label-entry label-name insts)
  (cons label-name insts)
)

(define (lookup-label labels label-name)
  (let ([val (assoc label-name labels)])
    (if val
      (cdr val)
      (error "Undefined label: ASSEMBLE" label-name)
    )
  )
)

(define (make-execution-procedure inst labels machine pc flag stack ops)
  (let ([inst-type (car inst)])
    (cond
      [(eq? inst-type 'assign) (make-assign inst machine labels ops pc)]
      [(eq? inst-type 'test) (make-test inst machine labels ops flag pc)]
      [(eq? inst-type 'branch) (make-branch inst machine labels flag pc)]
      [(eq? inst-type 'goto) (make-goto inst machine labels pc)]
      [(eq? inst-type 'save) (make-save inst machine stack pc)]
      [(eq? inst-type 'restore) (make-restore inst machine stack pc)]
      [(eq? inst-type 'perform) (make-perform inst machine labels ops pc)]
      [else (error "Unknown instruction type: ASSEMBLE" inst)]
    )
  )
)

(define (assign-reg-name inst)
  (cadr inst)
)

(define (assign-value-exp inst)
  (cddr inst)
)

(define (advance-pc pc)
  (set-contents! pc (cdr (get-contents pc)))
)

(define (make-assign inst machine labels operations pc)
  ;(display "make-assign: ") (display inst) (newline)
  (let ([target (get-register machine (assign-reg-name inst))]
        [value-exp (assign-value-exp inst)]) 
    (let ([value-proc
            (if (operation-exp? value-exp)
              (make-operation-exp value-exp machine labels operations)
              (make-primitive-exp (car value-exp) machine labels)
            )
          ])
      (lambda ()
        (set-contents! target (value-proc))
        (advance-pc pc)
      )
    )
  )
)

(define (make-test inst machine labels operations flag pc)
  ;(display "make-test ") (display inst) (newline)
  (let ([condition (test-condition inst)])
    ;(display "make-test-condition ") (display condition) (newline)
    (if (operation-exp? condition)
      (let ([condition-proc (make-operation-exp condition machine labels operations)])
        (lambda ()
          (set-contents! flag (condition-proc))
          (advance-pc pc)
        )
      )
      (error "Bad TEST instruction: ASSEMBLE" inst)
    )
  )
)

(define (test-condition inst)
  (cdr inst)
)

(define (make-branch inst machine labels flag pc)
  (let ([dest (branch-dest inst)])
    (if (label-exp? dest)
      (let ([insts (lookup-label labels (label-exp-label dest))]) 
        (lambda ()
          (if (get-contents flag)
            (set-contents! pc insts)
            (advance-pc pc)
          )
        )
      )
      (error "Bad BRANCH instruction: ASSEMBLE" inst)
    )
  )
)

(define (branch-dest inst)
  (cadr inst)
)

(define (make-goto inst machine labels pc)
  (let ([dest (goto-dest inst)])
    (cond 
      [(label-exp? dest)
        (let ([insts (lookup-label labels (label-exp-label dest))])
          (lambda ()
            (set-contents! pc insts)
          )
        ) 
      ]
      [(register-exp? dest)
        (let ([reg (get-register machine (register-exp-reg dest))])
          (lambda ()
            (set-contents! pc (get-contents reg))
          )
        )
      ]
      [else (error "Bad GOTO instruction: ASSEMBLE" inst)]
    )
  )
)

(define (goto-dest inst)
  (cadr inst)
)

(define (make-save inst machine stack pc)
  (let ([reg (get-register machine (stack-inst-reg-name inst))])
    (lambda ()
      (push stack (get-contents reg))
      (advance-pc pc)
    )
  )
)

(define (make-restore inst machine stack pc)
  (let ([reg (get-register machine (stack-inst-reg-name inst))])
    (lambda ()
      ;(display "make-restore: ") (display reg) (newline)
      (set-contents! reg (pop stack))
      (advance-pc pc)
    )
  )
)

(define (stack-inst-reg-name inst)
  (cadr inst)
)

(define (make-perform inst machine labels operations pc)
  (let ([action (perform-action inst)])
    (if (operation-exp? action)
      (let ([action-proc (make-operation-exp action machine labels operations)])
        (lambda ()
          (action-proc)
          (advance-pc pc)
        )
      )
      (error "Bad PERFORM instruction: ASSEMBLE" inst)
    )
  )
)

(define (perform-action inst)
  (cdr inst)
)

(define (make-primitive-exp exp machine labels)
  (cond
    [(constant-exp? exp)
      (let ([c (constant-exp-value exp)])
        (lambda () c) 
      )
    ]
    [(label-exp? exp)
      (let ([insts (lookup-label labels (label-exp-label exp))])
        (lambda () insts)
      )
    ]
    [(register-exp? exp)
      (let ([reg (get-register machine (register-exp-reg exp))])
        (lambda () (get-contents reg))
      )
    ]
    [else (error "Unknown expression type: ASSEMBLE" exp)]
  )
)

(define (register-exp? exp)
  (tagged-list? exp 'reg)
)

(define (register-exp-reg exp)
  (cadr exp)
)

(define (constant-exp? exp)
  (tagged-list? exp 'const)
)

(define (constant-exp-value exp)
  (cadr exp)
)

(define (label-exp? exp)
  (tagged-list? exp 'label)
)

(define (label-exp-label exp)
  (cadr exp)
)

(define (make-operation-exp exp machine labels operations)
  ;(display "make-operation-exp ") (display exp) (newline)
  (let ([op (lookup-prim (operation-exp-op exp) operations)]
        [aprocs
          (map
            (lambda (e)
              (make-primitive-exp e machine labels)
            )
            (operation-exp-operands exp)
          )
        ])
    (lambda ()
      ;(display "make-operation-exp-lambda ") (display op) (newline)
      (apply op (map (lambda (p) (p)) aprocs))
    )
  )
)

(define (operation-exp? exp)
  (and (pair? exp) (tagged-list? (car exp) 'op))
)

(define (operation-exp-op exp)
  (cadr (car exp))
)

(define (operation-exp-operands exp)
  (cdr exp)
)

(define (lookup-prim symbol operations)
  (let ([val (assoc symbol operations)])
    (if val 
      (cadr val)
      (error "Unknown operation: ASSEMBLE" symbol)
    )
  )
)

(define eceval-operations
  (list
    (list 'true? true?)
    (list 'false? false?)
    (list 'eof? eof?)
    (list 'read read)
    (list 'lookup-variable-value lookup-variable-value)
    (list 'set-variable-value! set-variable-value!)
    (list 'define-variable! define-variable!)
    (list 'get-global-environment get-global-environment)
    (list 'primitive-procedure? primitive-procedure?)
    (list 'compound-procedure? compound-procedure?)
    (list 'self-evaluating? self-evaluating?) 
    (list 'variable? variable?)
    (list 'quoted? quoted?)
    (list 'text-of-quotation text-of-quotation)
    (list 'assignment? assignment?)
    (list 'assignment-variable assignment-variable)
    (list 'assignment-value assignment-value)
    (list 'definition? definition?)
    (list 'definition-variable definition-variable)
    (list 'definition-value definition-value)
    (list 'if? if?)
    (list 'if-predicate if-predicate)
    (list 'if-alternative if-alternative)
    (list 'if-consequent if-consequent)
    (list 'lambda? lambda?)
    (list 'lambda-parameters lambda-parameters)
    (list 'lambda-body lambda-body)
    (list 'make-procedure make-procedure)
    (list 'begin? begin?)
    (list 'begin-actions begin-actions)
    (list 'no-more-exps? no-more-exps?)
    (list 'first-exp first-exp)
    (list 'rest-exps rest-exps)
    (list 'application? application?)
    (list 'operator operator)
    (list 'operands operands)
    (list 'empty-arglist empty-arglist)
    (list 'no-operands? no-operands?)
    (list 'first-operand first-operand)
    (list 'rest-operands rest-operands)
    (list 'last-operand? last-operand?)
    (list 'adjoin-arg adjoin-arg)
    (list 'apply-primitive-procedure apply-primitive-procedure)
    (list 'procedure-parameters procedure-parameters)
    (list 'procedure-environment procedure-environment)
    (list 'procedure-body procedure-body)
    (list 'extend-environment extend-environment)
    (list 'user-print user-print)
  )
) 
;---ends here

(define (make-new-machine) 
  (let ((pc (make-register 'pc)) 
        (flag (make-register 'flag)) 
        (stack (make-stack))
        (the-instruction-sequence '())) 
    (let ((the-ops
           (list (list 'initialize-stack
                       (lambda () (stack 'initialize)))))
          (register-table
           (list (list 'pc pc) (list 'flag flag)))) 
      (define (allocate-register name) 
        (if (assoc name register-table)
            (error "Multiply defined register: " name)
            (set! register-table
                  (cons (list name (make-register name))
                        register-table)))
        'register-allocated)
      (define (lookup-register name) 

        (let ((val (assoc name register-table)))
          (if val
              (cadr val)  

              (error "Unknown register:" name))))
      (define (execute)
        (let ((insts (get-contents pc)))  

          (if (null? insts)
              (void)
              (begin
                ((instruction-execution-proc (mcar insts))) 

                (execute)))))
      (define (dispatch message)
        (cond ((eq? message 'start)
               (set-contents! pc the-instruction-sequence) 

               (execute)) 

              ((eq? message 'install-instruction-sequence) 
               (lambda (seq) 
                 (set! the-instruction-sequence seq)
                 ))
              ((eq? message 'allocate-register) allocate-register)
              ((eq? message 'get-register) lookup-register)
              ((eq? message 'install-operations) 

               (lambda (ops) (set! the-ops (append the-ops ops))))
              ((eq? message 'stack) stack) 

              ((eq? message 'operations) the-ops) 
              (else (error "Unknown request -- MACHINE" message))))
      dispatch)))



(define (make-machine register-names ops controller-text);
  (let ((machine (make-new-machine)))
    (for-each (lambda (register-name)
                ((machine 'allocate-register) register-name))
              register-names)
    ((machine 'install-operations) ops)    
    ((machine 'install-instruction-sequence)
     (assemble controller-text machine))
    machine))

(define (start machine)
  (machine 'start))

;;;;;;;;;;;;;;;;;;;;;;;;;;****scheme machine controler


(define scheme-machine-controller 
'(

read-eval-print-loop
  (perform (op initialize-stack))
  ;(perform
   ;(op prompt-for-input) (const ";;; EC-Eval input:"))
  (assign exp (op read))
  (assign env (op get-global-environment))
  (assign continue (label print-result))  ;before doing somthing that may change the return address, always assign continue with right label
  (goto (label eval-dispatch))
print-result
;  (perform
;   (op announce-output) (const ";;; EC-Eval value:"))
  (perform (op user-print) (reg val)) ;the value of exp is stored in val
  (goto (label read-eval-print-loop))
  
  
  
eval-dispatch
;after this is completed, the value of exp is stored in reg val,and 
;program goto the address stored in reg continue;
; eval value of exp in env
  (test (op eof?) (reg exp)) ;addby guo wei
  (branch (label program-end))
  (test (op self-evaluating?) (reg exp))
  (branch (label ev-self-eval))
  (test (op variable?) (reg exp))  ;assign exp to val and then goto continue
  (branch (label ev-variable))
  (test (op quoted?) (reg exp))
  (branch (label ev-quoted))
  (test (op assignment?) (reg exp))
  (branch (label ev-assignment))
  (test (op definition?) (reg exp))
  (branch (label ev-definition))
  (test (op if?) (reg exp))
  (branch (label ev-if))
  (test (op lambda?) (reg exp))
  (branch (label ev-lambda))
  (test (op begin?) (reg exp))
  (branch (label ev-begin))
  (test (op application?) (reg exp))
  (branch (label ev-application))
  (goto (label unknown-expression-type))
  
ev-self-eval
  (assign val (reg exp))
  (goto (reg continue))
ev-variable
  (assign val (op lookup-variable-value) (reg exp) (reg env))
  (goto (reg continue))
ev-quoted
  (assign val (op text-of-quotation) (reg exp))
  (goto (reg continue))
ev-lambda
  (assign unev (op lambda-parameters) (reg exp))
  (assign exp (op lambda-body) (reg exp))
  (assign val (op make-procedure)
              (reg unev) (reg exp) (reg env))
  (goto (reg continue))
;just store the value of exp in reg val,and goto continue
;the value of a lambda is a function object, and it is stored also in val  

ev-assignment
  (assign unev (op assignment-variable) (reg exp))
  (save unev)                   ; save variable for later
  (assign exp (op assignment-value) (reg exp))
  (save env)
  (save continue)
  (assign continue (label ev-assignment-1))
  (goto (label eval-dispatch))  ; evaluate the assignment value stored in exp
ev-assignment-1
  (restore continue)
  (restore env)
  (restore unev)
  (perform
   (op set-variable-value!) (reg unev) (reg val) (reg env)) ;variable name is stored in uenv
  (assign val (const ok))
  (goto (reg continue))
  
 
  
  
ev-definition
  (assign unev (op definition-variable) (reg exp))
  (save unev)                   ; save variable for later
  (assign exp (op definition-value) (reg exp))
  (save env)
  (save continue)
  (assign continue (label ev-definition-1))
  (goto (label eval-dispatch))  ; evaluate the definition value
ev-definition-1
  (restore continue)
  (restore env)
  (restore unev)
  (perform
   (op define-variable!) (reg unev) (reg val) (reg env))
  (assign val (const ok))
  (goto (reg continue))
  
  
ev-if
  (save exp)                    ; save the whole if expression for later
  (save env)
  (save continue) ;after ev-if,should goto continue
  (assign continue (label ev-if-decide))
  (assign exp (op if-predicate) (reg exp))
  (goto (label eval-dispatch))  ; evaluate the predicate  
  
  
ev-if-decide
  (restore continue) ;the address to which we should go after the whole ev-if is done
  (restore env)  ;the env for the whole if exp
  (restore exp)  ;the whole if exp
  (test (op true?) (reg val))
  (branch (label ev-if-consequent))

ev-if-alternative
  (assign exp (op if-alternative) (reg exp))
  (goto (label eval-dispatch))
ev-if-consequent
  (assign exp (op if-consequent) (reg exp))
  (goto (label eval-dispatch))  

  

ev-begin
  (assign unev (op begin-actions) (reg exp))
  (save continue)
  (goto (label ev-sequence))
  
ev-sequence
  (test (op no-more-exps?) (reg unev))
  (branch (label ev-sequence-end))
  (assign exp (op first-exp) (reg unev))
  (save unev)
  (save env)
  (assign continue (label ev-sequence-continue))
  (goto (label eval-dispatch))
ev-sequence-continue
  (restore env)
  (restore unev)
  (assign unev (op rest-exps) (reg unev))
  (goto (label ev-sequence))
ev-sequence-end
  (restore continue)
  (goto (reg continue))  
  
  
  
  
ev-application
  (save continue)
  (save env)
  (assign unev (op operands) (reg exp))
  (save unev)
  (assign exp (op operator) (reg exp))
  (assign continue (label ev-appl-did-operator))
  (goto (label eval-dispatch))


ev-appl-did-operator
  (restore unev)                  ; the operands
  (restore env)
  (assign argl (op empty-arglist))
  (assign proc (reg val))         ; the operator,is a function object
  (test (op no-operands?) (reg unev))
  (branch (label apply-dispatch))
  (save proc)


ev-appl-operand-loop
  (save argl)
  (assign exp (op first-operand) (reg unev))
  (test (op last-operand?) (reg unev))
  (branch (label ev-appl-last-arg))
  (save env)
  (save unev) ;the oprands, in which the first one is going to be evaluated and can be discarded
  (assign continue (label ev-appl-accumulate-arg))
  (goto (label eval-dispatch))

ev-appl-accumulate-arg
  (restore unev)  ;operands, in which the fisrt one is already evaluated
  (restore env)
  (restore argl)
  (assign argl (op adjoin-arg) (reg val) (reg argl))
  (assign unev (op rest-operands) (reg unev))
  (goto (label ev-appl-operand-loop))

ev-appl-last-arg
  (assign continue (label ev-appl-accum-last-arg))
  (goto (label eval-dispatch))
ev-appl-accum-last-arg
  (restore argl)
  (assign argl (op adjoin-arg) (reg val) (reg argl))
  (restore proc)
  (goto (label apply-dispatch))



apply-dispatch
  (test (op primitive-procedure?) (reg proc))
  (branch (label primitive-apply))
  (test (op compound-procedure?) (reg proc))  
  (branch (label compound-apply))
  (goto (label unknown-procedure-type))





primitive-apply
  (assign val (op apply-primitive-procedure)
              (reg proc)
              (reg argl))
  (restore continue)
  (goto (reg continue))




compound-apply
  (assign unev (op procedure-parameters) (reg proc))
  (assign env (op procedure-environment) (reg proc))
  (assign env (op extend-environment)
              (reg unev) (reg argl) (reg env))
  (assign unev (op procedure-body) (reg proc))
  (goto (label ev-sequence))
                   
  
unknown-expression-type
  (assign val (const unknown-expression-type-error))
  (goto (label signal-error))
unknown-procedure-type
  (restore continue)    ; clean up stack (from apply-dispatch)
  (assign val (const unknown-procedure-type-error))
  (goto (label signal-error))
signal-error
  (perform (op user-print) (reg val))
  (goto (label read-eval-print-loop))
program-end ;addfor scheme-machine by guo wei

)
)  ;end of scheme-machine-controller
  


;;;;;;;;;;;;;;;;;;;;;;;;;;
(define scheme-machine
  (make-machine
   '(exp env val proc argl continue unev c d)
   eceval-operations
   scheme-machine-controller
  ))
(start scheme-machine)