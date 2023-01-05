;; Problem 1 

;; functions from class notes.
(define lambda?
  (lambda (exp)
    (and (list? exp)                 
			(equal? (length exp) 3)
			(equal? (car exp) 'lambda)
			(list? (cadr exp))
			(equal? (length (cadr exp)) 1)
			(symbol? (caadr exp)))))

(define var?
  (lambda (exp)
    (symbol? exp)))

(define apply?
  (lambda (exp)
    (and (list? exp)
			(equal? (length exp) 2))))

(define lambda-exp?	 
  (lambda (exp)
    (cond
			[(var? exp) #t]
			[(lambda? exp) (lambda-exp? (caddr exp))]
			[(apply? exp) (and (lambda-exp? (car exp)) (lambda-exp? (cadr exp)))]
			[else #f]
			)))

;; test cases.
(define lc1 'x )
(define lc2 'y )
(define lc3 '(lambda (x) x) )
(define lc4 '(lambda (y) x) )
(define lc5 '(x y) )
(define lc6 '(x (y y)) )
(define lc7 '(x (lambda (y) y)) )
(define lc8 '(lambda (x) (x y)) )
(define lc9 '(lambda (x) (lambda (y) x)) )
(define lc10 '((lambda (x) x) (lambda (y) y)) )

;; Question 1
(define get-lvars (lambda [exp]
    (get-lvars* exp '())
))

(define get-lvars* (lambda [exp var-list]
    (cond
			[(null? exp) '()]
            [(not (lambda-exp? exp)) "not a valid lambda calculus expression."]
            [(var? exp) (list exp)]
            [(lambda? exp) (get-lvars* (caddr exp) var-list)]
            [(apply? exp) (append (get-lvars* (car exp) var-list) (get-lvars* (cadr exp) var-list))]
)))

;; Question 2
(define get-lparams (lambda [exp]
    (get-lparams* exp '())
))

(define get-lparams* (lambda [exp param-list]
    (cond
			[(null? exp) '()]
            [(not (lambda-exp? exp)) "not a valid lambda calculus expression."]
            [(var? exp) param-list]
            [(lambda? exp) (get-lparams* (caddr exp) (append (cadr exp) param-list))]
            [(apply? exp) (append (get-lparams* (car exp) param-list) (get-lparams* (cadr exp) param-list))]
)))

;; Question 3
(define replace-vars (lambda [exp]
    (replace-vars* exp 0)
))

(define replace-vars* (lambda [exp count]
    (cond
			[(null? exp) '()]
            [(not (lambda-exp? exp)) "not a valid lambda calculus expression."]
            [(var? exp) count]
            [(lambda? exp) (list (car exp) (cadr exp) (replace-vars* (caddr exp) (+ count 1)))]
            [(apply? exp) (list (replace-vars* (car exp) count) (replace-vars* (cadr exp) count))]
)))

;; Question 4
(define free-vars
  (lambda (exp)
    (free exp '())))

(define free
  (lambda (exp formals-seen)
    (cond
            [(null? exp) '()]
            [(var? exp) (if (not (member exp formals-seen)) (list exp) '())]
            [(lambda? exp) (free (caddr exp) (append (cadr exp) formals-seen))]
            [(apply? exp) (append (free (car exp) formals-seen) (free (cadr exp) formals-seen))]
        )))