#lang racket
(require r5rs)

;---tagged list
(define (tagged-list? exp tag)
  (if (pair? exp)
    (eq? (car exp) tag)
    false
  )
)

;---primitive procedures

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
(define (eval exp env)
  (void)
)

;---apply procedures


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

;(driver-loop)
(display glb-env)