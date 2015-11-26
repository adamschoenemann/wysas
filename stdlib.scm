(define (not x) (if x #f #t))
(define (null? l) (if (eqv? l '()) #t #f))

(define (list . objs) objs)

(define (id obj) obj)

(define (flip func) (lambda (a1 a2) (func a2 a1)))

(define (curry func arg1) (lambda (arg) (apply func (cons arg1 (list arg)))))

(define (compose f g) (lambda arg (f (apply g arg))))

(define zero? (curry = 0))
(define positive? (curry < 0))
(define negative? (curry > 0))
(define (odd? num) (= (mod num 2) 1))
(define (even? num) (= (mod num 2) 0))

(define (foldr func lst end)
    (if (null? lst)
        end
        (func (car lst) (foldr func (cdr lst) end))))

(define (foldl func accum lst)
    (if (null? lst)
        accum
        (foldl func (func accum (car lst)) (cdr lst))))
