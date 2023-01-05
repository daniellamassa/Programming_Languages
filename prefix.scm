(define-datatype prefix-exp prefix-exp?
  [const-exp [num integer?]]
  [diff-exp [operand1 prefix-exp?]
            [operand2 prefix-exp?]])

;; Question 1
(define parse-prefix (lambda [exp]
  ;; I wanted to use prefix-exp? to check input validity here, but that predicate always returns false, even on valid inputs. 
  (parse-prefix* (reverse exp) '())))

(define parse-prefix*
  (lambda [exp stack]
      (cond
      [(null? exp) (car stack)]
      [(number? (car exp)) (parse-prefix* (cdr exp) (cons (const-exp (car exp)) stack))]
      [(eqv? (car exp) '-) (parse-prefix* (cdr exp) (cons (diff-exp (car stack) (cadr stack)) (cddr stack)))]
      [else (eopl:error 'parse-prefix "Concrete parse error with expression ~s" exp)]
      )))

;; Question 2
(define unparse-prefix
  (lambda [exp]
    (cases prefix-exp exp
     [const-exp [num] (list num)]
     [diff-exp [operand1 operand2]
	      (cons '- (append (unparse-prefix operand1) (unparse-prefix operand2)))])))

;; Question 3
(define eval-prefix
  (lambda [exp]
    (cases prefix-exp exp
     [const-exp [num] num]
     [diff-exp [operand1 operand2]
	      (- (eval-prefix operand1) (eval-prefix operand2))])))