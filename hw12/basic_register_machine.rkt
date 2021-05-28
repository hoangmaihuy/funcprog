#lang racket
(require r5rs)

(define current-machine '*unassigned*)

(define (make-machine register-names ops controller-text)
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
          (set! s (car s))
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
      (define (loopup-register name)
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
          [else (error "Unknown request: MACHINE")]
        )
      )
      dispatch
    )
  )
)

(define (input-loop)
  (let ([input (read)])
    (cond
      [(eq? input eof) (void)]
      [(eq? (car input) 'make-machine)
        (let ([register-names (cadr input)]
              [ops (caddr input)]
              [controller-text (cadddr input)])
          (display "a new machine") (newline)
          (set! 
            current-machine
            (make-machine register-names ops controller-text)
          )
          (input-loop)
        )
      ]
      [else 
        (void)
      ]
    )
  )
)

(input-loop)