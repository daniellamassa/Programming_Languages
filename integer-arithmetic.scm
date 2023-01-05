;; Problem 2

;; Question 1
(define zero
	(lambda () '(0)))

(define is-Zero?
	(lambda (n)
		(not (fold-left (lambda [prev el] (or (= el 1) prev)) #f n))))

(define successor
	(lambda (lst)
    (reverse (successor* (reverse lst)))
	))

(define successor* (lambda [lst]
   (cond
        [(null? lst) '(1)]
        [(equal? (car lst) 0) (cons 1 (cdr lst))]
        [(equal? (car lst) 1) (cons 0 (successor* (cdr lst)))]
   )
))

(define predecessor
	(lambda [lst]
		(if (is-Zero? lst) (zero) (reverse (predecessor* (reverse lst))))))
    
(define predecessor* (lambda [lst]
   (cond
        [(null? lst) '(0)]
        [(equal? (car lst) 1) (cons 0 (cdr lst))]
        [(equal? (car lst) 0) (cons 1 (predecessor* (cdr lst)))]
   )
))

;; Question 2
;; for a lot of these functions I should have used 'else' in my final condition statement, but I realized this after the fact.

(define binary->number (lambda [binary]
    (binary->number* binary 0)))

(define binary->number* (lambda [binary count]
   (cond
        [(is-Zero? binary) count]
        [(not(is-Zero? binary)) (binary->number* (predecessor binary) (+ count 1))]
)))

(define number->binary (lambda [num]
    (number->binary* num '(0))))

(define number->binary* (lambda [num binary]
   (cond
        [(= num 0) binary]
        [(not(= num 0)) (number->binary* (- num 1) (successor binary))]
)))

;; Question 3
(define equals? (lambda [a b]
   (cond
         [(and (is-Zero? a) (is-Zero? b)) #t]
         [(or (and (is-Zero? a) (not (is-Zero? b))) (and (is-Zero? b) (not (is-Zero? a)))) #f]
         [else (equals? (predecessor a) (predecessor b))]
         )))

(define less-than? (lambda [a b]
   (cond
         [(or (and (is-Zero? a) (is-Zero? b)) (and (is-Zero? b) (not (is-Zero? a)))) #f]
         [(and (is-Zero? a) (not (is-Zero? b))) #t]
         [else (less-than? (predecessor a) (predecessor b))]
         )))

(define sum
  (lambda (a b)
    (if (is-Zero? a) 
			b
			(sum (predecessor a) (successor b)))))

(define multiply
  (lambda (a b)
    (if (is-Zero? a) 
			(zero)
			(sum b (multiply (predecessor a) b)))))
