#lang racket

(define (power-set set)
  (if (null? set)
    (list '())
    (let ((sub-power-set (power-set (cdr set))))
      (begin 
        (append '()
                (map (lambda (x) (cons (car set) x)) sub-power-set)
                sub-power-set
        )
      )
    )
  )
)

(define (power-set-iter set n)
  (if (= n 1)
    (power-set set)
    (power-set (power-set-iter set (- n 1)))
  )
)

(define (main)
  (define set (read))
  (define n (read))
  (if (eq? set eof)
    (void)
    (begin
      (displayln (power-set-iter (sort (remove-duplicates set) <) n))
      (main)
    )
  )
)

(main)