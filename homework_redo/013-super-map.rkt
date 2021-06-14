#lang racket
(define (exit) #f)
(define (map op lst)
  (if (null? lst)
      '()
      (cons (op (car lst))
            (map op (cdr lst)))))
  
(define (super-map op . w)
;--- starts here
	(cond
		[(null? w) '()]
		[(null? (car w)) '()]
		[else
			(cons
				(apply op (map car w))
				(apply super-map (cons op (map cdr w)))
			)
		]
	)
)
;--- ends here
(define (myloop)
  (let ((a (read))
        (b (read))
        (c (read)))
    (if (eq? a eof)
        (void)
        (begin (displayln (super-map + a b c)) 
               (displayln (super-map (lambda (x y) (+ x (* 2 y) )) a b ))
               (myloop)))))
(myloop)