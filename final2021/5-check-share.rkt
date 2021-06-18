#lang racket
(require r5rs)
(define env (scheme-report-environment 5))

(define (check-share lst)
;-- starts
	(let ([visited '()])
		(define (check-loop l)
			(if (memq l visited)
				; (begin (displayln l) #t)
				#t
				(begin 
					(if (pair? l)
						(begin
							(set! visited (cons l visited))
							(or (check-loop (car l)) (check-loop (cdr l)))
						)
						#f
					)
				)
			)
		)
		(if (check-loop lst)
			(begin (display "True") (newline) (void))
			(begin (display "False") (newline) (void))
		)
	)
)
;-- ends
(displayln "**********")
(define lst1 (list 'a 'b))
(define lst2 (list 5))
(define pair1 (cons 1 12))
(define pair2 (cons 'cc 3))
(check-share lst1)
(check-share pair1)



(displayln "**********")
(check-share (cons lst1 lst1))
(check-share (list pair1 pair2))
(check-share (list (cons lst2 pair1) lst2))
(check-share (list (list pair2 (cons 1 12) pair1) (append lst1 lst2)))
(displayln "**********")



(define (myloop)
  (define declaration
    '((define lst1 (list 'a 'b))
      (define lst2 (list 5))
      (define pair1 (cons 1 12))
      (define pair2 (cons 'cc 3))))
  
  (define (eval-codes codes last-val)
    (if (null? codes)
        last-val
        (eval-codes (cdr codes) (eval (car codes) env))))
    
  (let ((codes (read)))
    (if (eq? codes eof)
        (void)
        (begin
          (check-share
           (eval-codes (append declaration codes) (void)))
          (myloop)))))


(myloop)