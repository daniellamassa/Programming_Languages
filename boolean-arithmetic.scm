;; Problem 3

;; true = /true/
;; false = /false/
;; (and-ba /a/ /b/) = /a and b/
;; (not-ba /a/) = /not a/
;; (xor-ba /a/ /b/) = /a exclusive-or b/
;; (if-ba /test/ exp1 exp2) = exp1 if test is true, and exp2 if test is false

(define true
  (lambda (a)
    (lambda (b)
      a)))

(define false
  (lambda (a)
    (lambda (b)
      b)))

(define if-ba 
  (lambda (test exp1 exp2)
    ((test exp1) exp2)))

(define not-ba 
   (lambda (a)
      (if-ba a false true)))

(define and-ba 
   (lambda (a b)
      (if-ba a (if-ba b true false) (if-ba b false true))))

(define xor-ba 
   (lambda (a b)
      (if-ba a (if-ba b false true) (if-ba b true false))))

(define ba->boolean 
   (lambda (ba)
      (if-ba ba #t #f)))

(define boolean->ba
   (lambda (bool)
      (if bool true false)))

