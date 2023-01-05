;; Exercise 1 GRADE
(define times10 (lambda [nums]
    (map (lambda (x) (* 10 x)) nums)
))

;; Exercise 2 GRADE
(define pair-up (lambda [elt ls]
    (map (lambda (x) (cons elt x)) ls)
))

;; Exercise 3 GRADE
(define x-odds (lambda [nums]
    (map (lambda (a) 
    (if (odd? a) 'x a)) nums)
))

;; Exercise 4 GRADE
(define replace (lambda [old new syms]
    (map (lambda (a) 
    (if (equal? a old) new a)) syms)
))

;; Exercise 5 GRADE
(define remove (lambda [elt ls]
    (filter (lambda [a] (not (equal? a elt))) ls)
))

;; Exercise 7 GRADE
(define length (lambda [ls]
    (fold-left (lambda [acc head] (+ acc 1)) 0 ls)
))

;; Exercise 8 GRADE
(define average (lambda [nums]
    (/ (fold-right (lambda [a b] (+ a b)) 0 nums))
    (length nums)
))





