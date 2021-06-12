#lang racket

(define (flatmap x)
  (cond 
    ((null? x) '())
    ((pair? x) (append '() (flatmap (car x)) (flatmap (cdr x))))
    (else (list x))
  )
)

(define (main)
  (define x (read))
  (if (eq? x eof)
    (void)
    (begin
      (displayln (flatmap x))
      (main)
    )
  )
)

(main)