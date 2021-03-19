#lang racket

(define (variable? exp) 
  (symbol? exp)
)

(define (sum? exp)
  (eq? (car exp) '+)
)

(define (product? exp)
  (eq? (car exp) '*)
)

(define (addend exp)
  (cadr exp)
)

(define (augend exp)
  (cddr exp)
)

(define (multiplier exp)
  (cadr exp)
)

(define (multiplicand) exp
  (cddr exp)
)

(define (make-sum exp1 exp2)
  (list '+ exp1 exp2)
)

(define (make-product exp1 exp2)
  (list '* exp1 exp2)
)

(define (deriv exp var)
  (cond (
    (number? exp) 0
    (variable? exp) (if (same-variable exp var) 1 0)
    (sum? exp) 
      (make-sum (deriv (addend exp) var) 
                (deriv (augend exp) var)
      )
    (product? exp) 
      (make-sum 
        (make-product 
          (multiplier exp)
          (deriv (multiplicand exp) var)
        )
        (make-product
          (deriv (multiplier exp) var)
          (multiplicand exp)
        )
      )
    (else (error "unknown expression type: DERIV" exp))
  )
)