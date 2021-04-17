#lang racket

(define exit #f)

(define cont-list '())
(define len (read))

(define (set-cont-list n)
  (define (build-continuation k)
    (define saved-cc #t)
    (if (call/cc (lambda (cc) 
                   (set! saved-cc cc)
                   #t
                 )
        )
        (void)
        (map (lambda (x) (displayln (+ x 1))) (reverse (range (+ k 1))))
    )
    saved-cc
  )

  (set! cont-list (map build-continuation (range n)))
)

(define (show n)
  (define (show-helper l n)
    (if (= n 0)
        (if (continuation? (car l))
            ((car l) #f)
            (displayln "error"))
        (show-helper (cdr l) (- n 1))))
  (show-helper cont-list (- n 1)))

(define (main)
  (set-cont-list len)
  (define k (read))
  (if (eq? k eof)
      (void)
      (show k)))

(main)