#lang racket

(define (accumulate op init seq)
  (if (null? seq)
      init
      (op (car seq) (accumulate op init (cdr seq)))))

(define (enumerate-interval a b)
  (if (> a b)
      '()
      (cons a (enumerate-interval (+ a 1) b))))

(define (flatmap proc seq)
  (accumulate append '() (map proc seq)))


(define (tri-num-list n s)
;--- starts here
	(flatmap 
		(lambda (i)
			(flatmap
				(lambda (j)
					(flatmap
						(lambda (k)
							(if (= (+ i j k) s)
								(list (list i j k))
								'()
							)
						)
						(enumerate-interval (+ j 1) n)
					)
				)
				(enumerate-interval (+ i 1) n)
			)
		)
		(enumerate-interval 1 n)
	)
)
;--- ends here
(define (myloop)
  (let ((n (read))
        (s (read)))
    (if (eq? n eof)
        (void)
        (begin (display (tri-num-list n s)) (newline) (myloop)))))

(myloop)