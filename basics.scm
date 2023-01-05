;; Dani Massa HW 1 Submission

;; Collaborators: 
;; James Heffernan
;; Manav Bilakhia 

;; Exercise 1
42

;; Exercise 2
3.1415

;; Exercise 3
"hello world"

;; Exercise 4
'("Let" "the" "Grecian" "dream" "of" "his" "sacred" "stream")

;; Exercise 5
(+ 3 4)

;; Exercise 6
(* (+ 17 (* 4 (/ 5 9))) (- 6.7 13))

;; Exercise 7
(+ 0 1 2 3 4 5 6 7)

;; Exercise 8
(define x "hello world")

;; Exercise 9
(list? 6)

;; Exercise 10
(lambda [y] y)

;; Exercise 11
(define identity (lambda [e] e))

;; Exercise 12
(identity x)

;; Exercise 13

(let [[a 2] [b 7] [c 18]] (/ (+ (* b -1) (sqrt (- (* b b) (* 4 a c)))) (* 2 a)))

;; Exercise 14
(define plus42 (lambda [e] (if (number? e) (+ e 42) "the answer to...")))

;; Exercise 15
(car (list 1 1 2 3 5))

;; Exercise 16
(cadddr '(1 1 2 3 5))

;; Exercise 17
(cons 1(cons 1(cons 2(cons 3 '(5)))))

;; Exercise 18
(cons 3 4)

;; Exercise 19
(cons(cons(cons 1 2)(cons 3(cons 4 '()))) 5)

;; Exercise 20
(and (or #t #f) #t)

;; Exercise 21
(or (not (equal? #t (not #f))) (and #f (not #t)) #f)

;; Exercise 22
((lambda [e] (if (string? e) 42 "no")) x)

;; Exercise 23
(define positive? (lambda [e] (cond [(and (number? e) (> e 0)) #t] [else #f])))

;; Exercise 24
(define numMonth->strMonth (lambda [n] (cond
  [(= n 1) "January"]
  [(= n 2) "February"]
  [(= n 3) "March"]
  [(= n 4) "April"]
  [(= n 5) "May"]
  [(= n 6) "June"]
  [(= n 7) "July"]
  [(= n 8) "August"]
  [(= n 9) "September"]
  [(= n 10) "October"]
  [(= n 11) "November"]
  [(= n 12) "December"]
  [else "does not compute."])))

;; Exercise 25
(define list-member? (lambda [e ls] (cond
  [(null? ls) #f]
  [(equal? e (car ls)) #t]
  [else (list-member? e (cdr ls))])))

;; Exercise 26
(define range (lambda [num1 num2] (cond
  [(> num1 num2) '()]
  [(= num1 num2) (list num1)]
  [else (cons num1 (range (if (< num1 num2) (+ num1 1) num1) num2))])))

;; Exercise 27
(define list-append (lambda [ls1 ls2] (cond
  [(null? ls1) ls2]
  [else (cons (car ls1) (list-append (cdr ls1) ls2))])))

;; Exercise 28
(define list-flatten (lambda [lls] (cond
  [(null? lls) '()]
  [else (append (car lls) (list-flatten (cdr lls)))])))

;; Exercise 29
(define list-map (lambda [fn ls] (cond
    [(null? ls) '()]
    [else (cons (fn (car ls)) (list-map fn (cdr ls)))])))

;; Exercise 30
(define list-filter (lambda [p? ls] (cond
  [(null? ls) '()]
  [(p? (car ls)) (cons (car ls) (list-filter p? (cdr ls)))]
  [else (list-filter p? (cdr ls))])))

;; Exercise 31
(define list-counts (lambda [p? ls] (cond
  [(null? ls) 0]
  [(p? (car ls)) (+ (list-counts p? (cdr ls)) 1)]
  [else (list-counts p? (cdr ls))])))

;; Exercise 32

;; Exercise 33
(define list-insert (lambda [num ls] (cond
  [(null? ls) (list num)]
  [(<= num (car ls)) (cons num ls)]
  [else (cons (car ls) (list-insert num (cdr ls)))])))

;; Exercise 34
(define list-insertion-sort (lambda [nums] (cond
  [(null? nums) (list )]
  [else (list-insert (car nums) (list-insertion-sort (cdr nums)))])))

;; Exercise 35

;; Exercise 36
(define list-merge (lambda [nums1 nums2] (cond
  [(null? nums1) nums2]
  [(null? nums2) nums1]
  [(< (car nums1) (car nums2)) (cons (car nums1) (list-merge (cdr nums1) nums2))]
  [else (cons (car nums2) (list-merge (cdr nums2) nums1))])))

;; Exercise 37

;; Exercise 38

;; Exercise 39