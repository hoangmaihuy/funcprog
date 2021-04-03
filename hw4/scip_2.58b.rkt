#lang racket

(define (variable? exp) 
  (symbol? exp)
)

(define (same-variable? e1 e2)
  (and (variable? e1) (variable? e2) (eq? e1 e2))
)

(define (reduce-exp exp)
  (if (= (length exp) 1) 
    (car exp)
    exp
  )
)

(define (sum? exp)
  (if (null? exp)
    #f
    (if (eq? (car exp) '+)
      #t
      (sum? (cdr exp))
    )
  )
)

(define (product? exp)
  (if (null? exp)
    #f
    (if (eq? (car exp) '*)
      #t
      (product? (cdr exp))
    )
  )
)

(define (addend exp)
  (define (addend-iter exp)
   (if (eq? (car exp) '+)
      '()
      (cons (car exp) (addend-iter (cdr exp)))
    )

  )
  (define x (addend-iter exp))
  (if (= (length x) 1)
    (car x)
    x
  )
)

(define (augend exp)
  (define (augend-iter exp)
    (if (eq? (car exp) '+)
      (cdr exp)
      (append '() (augend-iter (cdr exp)))
    )
  )
  (reduce-exp (augend-iter exp))
)

(define (multiplier exp)
  (define (multiplier-iter exp)
    (if (eq? (car exp) '*)
      '()
      (cons (car exp) (multiplier-iter (cdr exp)))
    )
  )
  (reduce-exp (multiplier-iter exp))
)

(define (multiplicand exp)
  (define (multiplicand-iter exp)
    (if (eq? (car exp) '*)
      (cdr exp)
      (append '() (multiplicand-iter (cdr exp)))
    )
  )
  (reduce-exp (multiplicand-iter exp))
)

(define (make-sum exp1 exp2)
  (cond 
    ((eq? exp1 0) exp2)
    ((eq? exp2 0) exp1)
    ((and (pair? exp1) (pair? exp2)) (append exp1 (list '+) exp2))
    ((pair? exp1) (append exp1 (list '+ exp2)))
    ((pair? exp2) (append (list exp1 '+) exp2))
    (else (list exp1 '+ exp2))
  )
)

(define (make-product exp1 exp2)
  (cond 
    ((or (eq? exp1 0) (eq? exp2 0)) 0)
    ((eq? exp1 1) exp2)
    ((eq? exp2 1) exp1)
    (else (list exp1 '* exp2))
  )
)

(define (deriv exp var)
  (cond ((number? exp ) 0)
        ((variable? exp)
         (if (same-variable? exp var) 1 0))
        ((sum? exp)
         (make-sum (deriv (addend exp) var)
                   (deriv (augend exp) var)))
        ((product? exp)
         (make-sum 
          (make-product (multiplier exp)
                        (deriv (multiplicand exp) var))
          (make-product (deriv (multiplier exp) var)
                        (multiplicand exp))))
        (else 
         (error "wrong format"))))

(define (myloop)
  (let ((a (read)))
    (if (eq? a eof)
        (void)
        (begin (display (deriv a 'x)) (newline) (myloop)))))

(myloop)