#lang racket
(require r5rs)

(define current-machine '*unassigned*)

(define (tagged-list? exp tag)
  (if (pair? exp)
    (eq? (car exp) tag)
    false
  )
)

(define (make-machine register-names ops controller-text)
  ;(display "make-machine: ") (display ops) (newline)
  (let ([machine (make-new-machine)])
    (for-each
      (lambda (register-name)
        ((machine 'allocate-register) register-name)
      )
      register-names
    )
    ((machine 'install-operations) ops)
    ((machine 'install-instruction-sequence) (assemble controller-text machine))
    machine
  )
)

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

(define (make-new-machine)
  (let ([pc (make-register 'pc)]
        [flag (make-register 'flag)]
        [stack (make-stack)]
        [the-instruction-sequence '()])
    (let ([the-ops (list (list 'initialize-stack (lambda () (stack 'initialize))))]
          [register-table (list (list 'pc pc) (list 'flag flag))])
      (define (allocate-register name)
        (if (assoc name register-table)
          (error "Multiply defined register: " name)
          (set!
            register-table
            (cons (list name (make-register name)) register-table)
          )
        )
      )
      (define (lookup-register name)
        (let ([val (assoc name register-table)])
          (if val
            (cadr val)
            (error "Unknown register: " name)
          )
        )
      )
      (define (execute)
        (let ([insts (get-contents pc)])
          (if (null? insts)
            (void)
            (begin
              ;(display "execute: ") (display (car insts)) (newline)
              ((instruction-execution-proc (car insts)))
              (execute)
            )
          )
        )
      )
      (define (dispatch message)
        (cond
          [(eq? message 'start)
            (set-contents! pc the-instruction-sequence)
            (execute)
          ]
          [(eq? message 'install-instruction-sequence)
            (lambda (seq)
              (set! the-instruction-sequence seq)
            )
          ]
          [(eq? message 'allocate-register) allocate-register]
          [(eq? message 'get-register) lookup-register]
          [(eq? message 'install-operations) 
            (lambda (ops)
              (set! the-ops (append the-ops ops))
            ) 
          ]
          [(eq? message 'stack) stack]
          [(eq? message 'operations) the-ops]
          [else (error "Unknown request: MACHINE" message)]
        )
      )
      dispatch
    )
  )
)

(define (start machine)
  (machine 'start)
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
  ;(display "lookup-prim ") (display operations) (newline)
  (let ([val (assoc symbol operations)])
    ;(display "lookup-prim") (display symbol) (display " = ") (display val) (newline)
    (if val 
      (cadr val)
      (error "Unknown operation: ASSEMBLE" symbol)
    )
  )
)

(define (get-ops-from-text text-ops)
  (map
    (lambda (p)
      (let ([op-type (cadr (cadr p))]
            [op-func (caddr p)])
        ;(display "op-type: ") (display op-type) (newline)
        ;(display "op-func: ") (display op-func) (newline)
        (cond
          [(eq? op-func 'remainder) (list op-type remainder)]
          [(eq? op-func '=) (list op-type =)]
          [(eq? op-func '+) (list op-type +)]
          [(eq? op-func '-) (list op-type -)]
          [(eq? op-func '*) (list op-type *)]
          [(eq? op-func '/) (list op-type /)]
          [(eq? op-func '>) (list op-type >)]
          [(eq? op-func '<) (list op-type <)]
          [(eq? op-func 'eq?) (list op-type eq?)]
          [else (error "Unknown op: " p)]
        )
      )
    )
    (cdr text-ops)
  )
)

(define (input-loop)
  (let ([input (read)])
    (cond
      [(eq? input eof) (void)]
      [(eq? (car input) 'make-machine)
        (let ([register-names-quoted (cadr input)]
              [ops (caddr input)]
              [controller-text-quoted (cadddr input)])
          (display "a new machine") (newline)
          (set! 
            current-machine
            (make-machine 
              (cadr register-names-quoted) 
              (get-ops-from-text ops) 
              (cadr controller-text-quoted)
            )
          )
          (input-loop)
        )
      ]
      [else 
        (for-each
          (lambda (p)
            (let ([reg-name (car p)]
                  [reg-value (cadr p)])
              (let ([reg (get-register current-machine reg-name)])
                (set-contents! reg reg-value)
              )
            )
          )
          input
        )
        (start current-machine)
        (set! input (read))
        (for-each
          (lambda (reg-name)
            (let ([reg (get-register current-machine reg-name)])
              (display (get-contents reg)) (display " ")
            )
          )
          input
        )
        (newline)
        (input-loop)
      ]
    )
  )
)

(input-loop)
