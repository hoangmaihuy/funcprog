#lang racket

(define (variable? exp) 
  (symbol? exp)
)

(define (same-variable? e1 e2)
  (and (variable? e1) (variable? e2) (eq? e1 e2))
)

(define (sum? exp)
  (eq? (car exp) '+)
)

(define (product? exp)
  (eq? (car exp) '*)
)

(define (exponentiation? exp)
  (eq? (car exp) '**)
)

(define (addend exp)
  (cadr exp)
)

(define (augend exp)
  (caddr exp)
)

(define (multiplier exp)
  (cadr exp)
)

(define (multiplicand exp)
  (caddr exp)
)

(define (base exp)
  (cadr exp)
)

(define (exponent exp)
  (caddr exp)
)

(define (make-sum exp1 exp2)
  (cond 
    ((eq? exp1 0) exp2)
    ((eq? exp2 0) exp1)
    (else (list '+ exp1 exp2))
  )
)

(define (make-product exp1 exp2)
  (cond 
    ((or (eq? exp1 0) (eq? exp2 0)) 0)
    ((eq? exp1 1) exp2)
    ((eq? exp2 1) exp1)
    (else (list '* exp1 exp2))
  )
)

(define (make-exponentiation base exponent)
  (cond 
    ((eq? exponent 0) 1)
    ((eq? exponent 1) base)
    (else (list '** base exponent))
  )
)

(define (deriv exp var)
  (cond 
    ((number? exp) 0)
    ((variable? exp) (if (same-variable? exp var) 1 0))
    ((exponentiation? exp)
      (make-product 
        (make-product (exponent exp) 
                      (make-exponentiation (base exp) (- (exponent exp) 1)))
        (deriv (base exp) var)
      )
    )
    ((sum? exp) 
      (make-sum (deriv (addend exp) var) 
                (deriv (augend exp) var)
      )
    )
    ((product? exp) 
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
    )
    (else (error "unknown deriv expression: " exp))
  )
)

(deriv '(+ x 3) 'x)
(deriv '(* 2 x) 'x)
(deriv '(* x y) 'x)
(deriv '(* (* x y) (+ x 3)) 'x)
(deriv '(** x 5) 'x)
(deriv '(* (** y 4) (** (+ x 1) 5)) 'x)