#lang racket
(define (cont-frac-iter N D k)
; submit from here
  (define (cont-frac-loop frac i)
    (if (= i 0)
      frac 
      (cont-frac-loop (/ (N i) (+ (D i) frac)) (- i 1))
    )
  )

  (cont-frac-loop 0 k)
)
; to here
(cont-frac-iter (lambda (x) x) 
           (lambda (x) 1.0)
           30)
 
(cont-frac-iter (lambda (x) (* 2 x))
           (lambda (x) (* 1.0 x))
           30)

(display "********") (newline)
(define (myloop)
  (let ((k (read)))
    (if (eq? k eof)
        (void)
        (begin (display (cont-frac-iter (lambda (x) 1.0) (lambda (x) 1.0) k)) 
(newline) (myloop)))))

(myloop)