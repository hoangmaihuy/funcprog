#lang racket

(define step 0.001)

(define (pow x b)
  (if (= b 0)
    1
    (* x (pow x (- b 1)))
  )
)

(define (square x) (* x x))

(define (eval-expr a b x)
  (if (null? a)
    0
    (+ (* (car a) (pow x (car b))) (eval-expr (cdr a) (cdr b) x))
  )
)

(define (range-iter L R step)
  (if (> L R)
    (list)
    (append (list L) (range-iter (+ L step) R step))
  )
)

(define (area-under-curve a b L R)
  (define (cal-area points)
    (if (null? points)
      0
      (+ (* step (eval-expr a b (car points))) (cal-area (cdr points)))
    )
  )

  (cal-area (range-iter L R step))
)

(define (volume-revolving-curve a b L R)
  (define (cal-volume points)
    (if (null? points)
      0
      (+ 
        (* (* pi step) (square (eval-expr a b (car points)))) 
        (cal-volume (cdr points))
      )
    )
  )

  (cal-volume (range-iter L R step))
)

(define a (map string->number (string-split (read-line))))
(define b (map string->number (string-split (read-line))))
(define L (read))
(define R (read))

(displayln (area-under-curve a b L R))
(displayln (volume-revolving-curve a b L R))