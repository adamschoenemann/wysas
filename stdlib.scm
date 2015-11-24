(define (not x) (if x #f #t))
(define (null? l) (if (eqv? l '()) #t #f))

(define (list . objs) objs)

