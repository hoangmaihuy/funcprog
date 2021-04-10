#lang racket
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;以下是put和get的实现，不须搞明白也能完成本题
(require scheme/mpair)
(require racket/trace)
(define (make-table)
  (let ((local-table (mlist '*table*)))
    (define (lookup key-1 key-2)
      (let ((subtable (massoc key-1 (mcdr local-table))))
        (if subtable
            (let ((record (massoc key-2 (mcdr subtable))))
              (if record
                  (mcdr record)
                  false))
            false)))
    (define (insert! key-1 key-2 value)
      (let ((subtable (massoc key-1 (mcdr local-table))))
        (if subtable
            (let ((record (massoc key-2 (mcdr subtable))))
              (if record
                  (set-mcdr! record value)
                  (set-mcdr! subtable
                            (mcons (mcons key-2 value)
                                  (mcdr subtable)))))
            (set-mcdr! local-table
                      (mcons (mlist key-1
                                  (mcons key-2 value))
                            (mcdr local-table)))))
      (void))    
    (define (dispatch m)
      (cond ((eq? m 'lookup-proc) lookup)
            ((eq? m 'insert-proc!) insert!)
            (else (error "Unknown operation -- TABLE" m))))
    dispatch))

(define operation-table (make-table))
(define get (operation-table 'lookup-proc))
(define put (operation-table 'insert-proc!))

(define conversion-table (make-table))
(define get-coercion (conversion-table 'lookup-proc))
(define put-coercion (conversion-table 'insert-proc!))
;以上是put和get的实现，不须搞明白也能完成本题
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;------------- integer package
(define (install-integer-package)
  (define (tag x)
    (attach-tag 'integer x))    
  (put 'add '(integer integer)
       (lambda (x y) (tag (+ x y))))
  (put 'sub '(integer integer)
       (lambda (x y) (tag (- x y))))
  (put 'mul '(integer integer)
       (lambda (x y) (tag (* x y))))
  (put 'div '(integer integer)
       (lambda (x y) ((get 'make 'rational )  x y)))
  (put 'make 'integer
       (lambda (x) (tag x)))
  (void))

(define (make-integer n)
  ((get 'make 'integer) n))


;--------general functions
  
(define (attach-tag type-tag contents)
  (cons type-tag contents))
(define (type-tag datum)
  (if (pair? datum)
      (car datum)
      (error "Bad tagged datum -- TYPE-TAG" datum)))
(define (contents datum)
  (if (pair? datum)
      (cdr datum)
      (error "Bad tagged datum -- CONTENTS" datum)))

;---------code starts here
; =zero? for integer
(put '=zero? '(integer) (lambda (x) (= x 0)))

(define (install-polynomial-package)
  ;; internal procedures
  ;; representation of poly
  (define variable-list '(a b c d e))
  (define (index-of lst elem)
    (if (null? elem)
      0
      (if (eq? elem (car lst)) 
        0
        (+ 1 (index-of (cdr lst) elem))
      )
    )
  )
  (define (rank-variable x) (index-of variable-list x))
  (define (rank-higher? x y) (< (rank-variable x) (rank-variable y)))
  (define (rank-equal? x y) (= (rank-variable x) (rank-variable y)))
  (define (rank-lower? x y) (> (rank-variable x) (rank-variable y)))
  (define (make-poly variable term-list) (cons variable term-list))
  (define (variable p) (car p))
  (define (term-list p) (cdr p))
  (define (variable? x) (symbol? x))
  (define (same-variable? x y) (and (variable? x) (variable? y) (eq? x y)))
  ;; representation of terms and term lists
  (define (adjoin-term term term-list)
    (cons term term-list))
  (define (the-empty-termlist) '())
  (define (first-term term-list) (car term-list))
  (define (rest-terms term-list) (cdr term-list))
  (define (empty-termlist? term-list) (null? term-list))
  (define (make-term order coeff) (list order coeff))
  (define (order term) (car term))
  (define (coeff term) (cadr term))

  (define (add-terms L1 L2)
    (cond ((empty-termlist? L1) L2)
          ((empty-termlist? L2) L1)
          (else
            (let ((t1 (first-term L1))
                 (t2 (first-term L2)))
              (cond ((> (order t1) (order t2))
                      (adjoin-term
                        t1 (add-terms (rest-terms L1) L2)))
                    ((< (order t1) (order t2))
                      (adjoin-term
                        t2 (add-terms L1 (rest-terms L2))))
                    (else
                      (adjoin-term
                        (make-term (order t1)
                                   (add (coeff t1) (coeff t2)))
                        (add-terms (rest-terms L1)
                                   (rest-terms L2)))))))))

  (define (mul-terms L1 L2)
    (if (empty-termlist? L1)
      (the-empty-termlist)
      (add-terms (mul-term-by-all-terms (first-term L1) L2)
                 (mul-terms (rest-terms L1) L2))))

  (define (mul-term-by-all-terms t1 L)
    (if (empty-termlist? L)
      (the-empty-termlist)
      (let ((t2 (first-term L)))
        (adjoin-term
          (make-term (+ (order t1) (order t2))
                     (mul (coeff t1) (coeff t2)))
          (mul-term-by-all-terms t1 (rest-terms L))))))
  
  (define (poly->term-list var p)
    (make-poly var (list (list 0 (tag p))))
  )

  (define (add-poly p1 p2)
    ;(trace add-poly)
    (if (rank-equal? (variable p1) (variable p2))
      (make-poly (variable p1)
        (add-terms (term-list p1) (term-list p2)))
      (if (rank-higher? (variable p1) (variable p2))
        (add-poly p1 (poly->term-list (variable p1) p2))
        (add-poly p2 p1)
      )
    )
  )
  
  (define (mul-poly p1 p2)
    (if (rank-equal? (variable p1) (variable p2))
      (make-poly (variable p1)
        (mul-terms (term-list p1) (term-list p2)))
      (if (rank-higher? (variable p1) (variable p2))
        (mul-poly p1 (poly->term-list (variable p1) p2))
        (mul-poly p2 p1)
      )  
    )
  )

  (define (make-polynomial var terms)
    ((get 'make 'polynomial) var terms))

  (define (make-coeff coeff)
    ;(trace list->poly)
    (if (number? coeff)
      (make-integer coeff)
      (tag (list->poly coeff))
    )
  )

  (define (list->term-list terms)
    (if (empty-termlist? terms)
      (the-empty-termlist)
      (cons
        (make-term (order (first-term terms)) 
                   (make-coeff (coeff (first-term terms)))
        )
        (list->term-list (rest-terms terms))
      )
    )
  )

  (define (list->poly e)
    (make-poly 
      (variable e)
      (list->term-list (term-list e))
    )
  )

  (define (coeff->primitive coeff)
    (if (eq? (type-tag coeff) 'integer)
      (contents coeff)
      (poly->list (contents coeff))
    )
  )

  (define (term-list->list terms)
    (if (empty-termlist? terms)
      (the-empty-termlist)
      (cons
        (list (order (first-term terms)) 
              (coeff->primitive (coeff (first-term terms)))
        )
        (term-list->list (rest-terms terms))
      )
    )
  )
  (define (poly->list p)
    ;(trace term-list->list)
    (cons (variable p) (term-list->list (term-list p)))
  )

  (define (integer->poly var x)
    (make-poly var (list (list 0 (make-integer x))))
  )

  ;; interface to rest of the system
  (define (tag p) (attach-tag 'polynomial p))
  (put '=zero? '(polynomial)
    (lambda (p) 
      (and 
        (= 0 (order (first-term (term-list p))))
        (=zero? (coeff (first-term (term-list p))))
      )
    )
  )

  (put 'add '(polynomial polynomial)
  (lambda (p1 p2) (tag (add-poly p1 p2))))

  (put 'mul '(polynomial polynomial)
  (lambda (p1 p2) (tag (mul-poly p1 p2))))  

  (put 'add '(integer polynomial)
  (lambda (p1 p2) (tag (add-poly (integer->poly (variable p2) p1) p2))))

  (put 'mul '(integer polynomial)
  (lambda (p1 p2) (tag (mul-poly (integer->poly (variable p2 )p1) p2))))  

  (put 'add '(polynomial integer)
  (lambda (p1 p2) (tag (add-poly p1 (integer->poly (variable p1) p2)))))

  (put 'mul '(polynomial integer)
  (lambda (p1 p2) (tag (mul-poly p1 (integer->poly (variable p1) p2)))))

  (put 'make 'polynomial
  (lambda (var terms) (tag (make-poly var terms))))
  (put 'list->poly 'polynomial
    (lambda (e) (tag (list->poly e)))
  )
  (put 'poly->list 'polynomial 
    (lambda (p) (poly->list (contents p)))
  )
)

(define (apply-generic op . args)
  (let ((type-tags (map type-tag args)))
  (let ((proc (get op type-tags)))
    (if proc
      (apply proc (map contents args))
      (error
        "No method for these types: APPLY-GENERIC"
        (list op type-tags))))))

(define (build-poly e)
  ((get 'list->poly 'polynomial) e)
)

(define (display-poly p)
  (displayln ((get 'poly->list 'polynomial) p))
)

(define (=zero? x) (apply-generic '=zero? x))
;---------code ends here

(install-integer-package)
(install-polynomial-package)
(define (add x y) (apply-generic 'add x y))
(define (mul x y) (apply-generic 'mul x y))


(define (make-poly var terms)
  ((get 'make 'polynomial) var terms))
(define (make-term order coeff) 
  ((get 'make 'polynomial-term) order coeff))

(displayln "******1")
(define e1 (make-poly 'a (list (list 1 (make-integer 3)) (list 0 (make-integer 2))))) ;3a+2
(define e2 (make-poly 'a (list (list 2 (make-integer 4)) (list 1 (make-integer 3))))) ;4a^2 + 3a
(displayln e1)
(displayln e2)
(displayln (add e1 e2))
(displayln (mul e1 e2))

(displayln "******2")

(define c1 (make-poly 'b (list (list 1 (make-integer 3)) (list 0 (make-integer 2))))) ;3b+2
(define c2 (make-poly 'b (list (list 2 (make-integer 4)) (list 1 (make-integer 3))))) ;4b^2 + 3b

(define e3 (make-poly 'a (list (list 1 c1) (list 0 (make-integer 2))))) 
(define e4 (make-poly 'a (list (list 2 (make-integer 4)) (list 1 c2)))) 
(displayln (add e3 e4))

(displayln "******")
(define (myloop)
  (let ((a (read)))
    (if (eq? a eof)
        (void)
        (let ((op (car a))
              (e1 (cadr a))
              (e2 (caddr a)))
          (if (eq? op '+)
              (display-poly (add (build-poly e1) (build-poly e2)))
              (display-poly (mul (build-poly e1) (build-poly e2))))
          (myloop)))))
              
(myloop)