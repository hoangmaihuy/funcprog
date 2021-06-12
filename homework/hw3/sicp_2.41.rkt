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
  (define (map-i i)

		(define (map-j j) 

			(define (map-k k)
				(list i j k)
			)

			(map map-k (enumerate-interval (+ j 1) n))
		)

		(flatmap map-j (enumerate-interval (+ i 1) (- n 1)))
	)
	
	(define (ordered-and-equal-sum? triple)
	  (and (< (car triple) (cadr triple))
		     (< (cadr triple) (caddr triple))
				 (= s (+ (car triple) (cadr triple) (caddr triple)))
		)
	)

	(filter ordered-and-equal-sum? (flatmap map-i (enumerate-interval 1 (- n 2))))
)

(define (myloop)
  (let ((n (read))
        (s (read)))
    (if (eq? n eof)
        (void)
        (begin (display (tri-num-list n s)) (newline) (myloop)))))

(myloop)