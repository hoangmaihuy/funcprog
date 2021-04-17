#lang racket
(require r5rs)
(define (exit) #f)
(define env (scheme-report-environment 5))

(eval '(define (last-pair lst)
         (if (null? (cdr lst))
             lst
             (last-pair (cdr lst))))
      env)

(eval '(define (make-cycle lst)
         (set-cdr! (last-pair lst) lst)
         lst)
      env)

(eval '
  (define (check-cycle lst)
    (define visited '())
    (define (check-cycle-iter l)
        (cond 
            [(null? l) #f]
            [(memq (car l) visited) #t]
            [else (begin 
                    (set! visited (cons (car l) visited))
                    (check-cycle-iter (cdr l))
                  )
            ]
        )
    )
    (check-cycle-iter lst)
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