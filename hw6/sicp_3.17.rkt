#lang racket
(require r5rs)

(define env (scheme-report-environment 5))
(eval  ;only after evaluating your count-pairs, the program in the input can use count-pairs
 '
  (define (count-pairs p)
    (define pair-list '())
    (define (count-pairs-iter p)
      (if (and 
            (pair? p) 
            (eq? (memq p pair-list) #f)
          )
        (begin 
          (set! pair-list (cons p pair-list))
          (count-pairs-iter (car p))
          (count-pairs-iter (cdr p))
        )
				0
      )
    )
    (count-pairs-iter p)
		(length pair-list)
  ) 

env)

(define (myloop)
  (define (eval-codes codes last-val)
    (if (null? codes)
        last-val
        (eval-codes (cdr codes) (eval (car codes) env))))
    
  (let ((codes (read)))
    (if (eq? codes eof)
        (void)
        (begin (displayln (eval-codes codes (void))) (myloop)))))


(myloop)