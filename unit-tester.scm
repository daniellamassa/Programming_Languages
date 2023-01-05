;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;; -- Unit Test Framework --
;;
;; This is a simple harness for unit testing Scheme code.  The basic
;; usage is to add tests in the file you are testing via the function
;; add-my-test!, and then execute the tests after loading the file using
;; (run-all-tests!).
;;
;; Note that there is a more elaborate test harness included with the
;; course software already in the file test-harness.scm.
;;
;; Feel free to use this code or a modified version to test your
;; homework assignments.
;;
;; Author: Matthew Anderson  (F14, W17, F22)
;;

;; TO DO: 
;; - Add the capacity to have the individual tests broken into exercises, which have an associated name and point value. 
;; - When the command (run-all-tests!) is evaluated it should run all tests, grouping those in the same exercise together with the exercises created first being evaluated first. 
;; - The function should also tabulate and display the scores of the tests passed and the total number of points possible.
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; add batch test adds multiple tests, see problem 2 #1
;; Global variable for storing a list of tests, initially empty.
(define my-tests! '())

(define clear-tests! (lambda () (set! my-tests! '())))

;; (add-my-test! name-str qe1 qe2)
;; Function which takes a string name-str naming a test, and two
;; quoted S-expressions.  This function combines the name and the two
;; quoted expressions into a list and adds it to the head of the
;; global variable my-tests!  
;; MAKE SURE USE QUOTED EXPRESSIONS!
(define add-my-test!
  (lambda (test-name-str ex-name-str ptval qe1 qe2)
    (set! my-tests! (cons (list test-name-str ex-name-str ptval qe1 qe2) my-tests!))))

;; (display-result! val1 val2)
;; Takes two values and displays them.
(define display-result!
  (lambda (val1 val2)
    (display val1)
    (display " => ")
    (display val2)))

;; (display-test-success! name-str qe1 qe2 val1 val2)
;; Displays text to indicate test success.
(define display-test-success!
  (lambda (name-str qe1 qe2 val1 val2)
    (display name-str)
    (display " -- Success -- ")
    (display-result! qe1 qe2)
    (display "\n")))

;; (display-test-failure! name-str qe1 qe2 val1 val2)
;; Displays text to indicate failure.
(define display-test-failure!
  (lambda (name-str qe1 qe2 val1 val2)
    (display name-str)
    (display " -- Failure\n")
    (display "  Expected: ")
    (display-result! qe1 qe2)
    (display "\n    Actual: ")
    (display-result! qe1 val1)
    (display "\n            ")
    (display-result! qe2 val2)
    (display "\n")))

;; (run-one-test! name-str qe1 qe2)
;; Runs a test with the given name two quoted expressions
(define run-one-test!
  (lambda (name-str qe1 qe2)
    (guard
     (except [#t
	      (display name-str)
	      (display " -- Failure -- ")
	      (display-result! qe1 qe2)
	      (display " -- Exception!\n")])
     (let 
	 ([val1 (eval qe1)]  ;; This is why the quote are necessary.
	  [val2 (eval qe2)])
  (cond
	[(equal? val1 val2) ((lambda ()
                        (display-test-success! name-str qe1 qe2 val1 val2)
                        #t
  ))]
	[else ((lambda ()
          (display-test-failure! name-str qe1 qe2 val1 val2)
          #f
  ))]
  )))))

;; test-name-str ex-name-str ptval qe1 qe2
(define run-one-exercise!
  (lambda (ex-name-str test-ls)
    (run-one-exercise!* 0 0 ex-name-str test-ls))
  )

  (define run-one-exercise!* 
    (lambda (passed-points total-points ex-name-str test-ls)
    (if (not (null? test-ls))
    (let 
      [(current-test (car test-ls))]
      (let 
        [[test-name-str (car current-test)]
        [test-ex-name (cadr current-test)]
        [ptval (caddr current-test)]
        [qe1 (cadddr current-test)]
        [qe2 (cadr(cdddr current-test))]]
        (if (equal? ex-name-str test-ex-name) 
        (if (run-one-test! test-name-str qe1 qe2) 
          (run-one-exercise!* (+ passed-points ptval) (+ total-points ptval) ex-name-str (cdr test-ls)) 
        (run-one-exercise!* passed-points (+ total-points ptval) ex-name-str (cdr test-ls)))
        )
      )
    )
  ((lambda () 
  (display "Passed Points: ")
  (display passed-points)
  (display "\n")
  (display "Total Points: ")
  (display total-points)
  (display "\n")
  )))
  ))

(define add-batch-tests!
  (lambda (ex-name-str q-tests)
  (if (not (null? q-tests))
  (let 
    [[qe1 (car q-tests)]
    [qe2 (caddr q-tests)]]
    (add-my-test! "" ex-name-str 1 qe1 qe2)
    (add-batch-tests! ex-name-str (cdddr q-tests))
  ))
  ))

;; (run-all-tests!)  
;; Runs all tests.  Note this is a 0-ary function, i.e., it takes no
;; arguments.
(define run-all-tests!
  (lambda ()
    (run-all-tests!* (reverse my-tests!))))

;; (run-all-tests!* ls)
;; Recursive function to recurse through tests running each one
(define run-all-tests!* 
  (lambda (ls)
    (if (not (null? ls))
	(let
	    ([test (car ls)])
	  (let ([name-str (car test)]
		[qe1 (cadr test)]
		[qe2 (caddr test)])
	    (run-one-test! name-str qe1 qe2)
	    (run-all-tests!* (cdr ls)))))))

  
;; Sample tests for functions we wrote above
;;(add-my-test! "Reverse test" "Exercise 1" 2 '(reverse '(1 2 3)) ''(3 2 1))
;;(add-my-test! "Fib test" "Exercise 1" 5 '(reverse '(1 2 3 4 5)) ''(4 4 3 2 1))
;;(run-one-exercise! "Exercise 1" my-tests!)
;;(add-my-test! "Fib test *SHOULD FAIL*" '(fib 5) ''(1 3 4)) ;; should fail

(add-batch-tests! "Exercise 1 (2pts)" '(
(times10 '(1 2 3 4 5))  =>  '(10 20 30 40 50)
(times10 '(25))  =>  '(250)
))

;; MATT: for some reason run-one-exercise! works, but run-all-tests! does not. If you use run-one-exercise! you'll see that my tests pass. 
;; I had originally fully implemented part 4, but I deleted it because I didn't realize I had this bug, and I tried to fix by retracing my steps. 
;; In the end I wasn't able to resolve this bug, and I also completely lost my implementation for part 4. However, I included the logic I had 
;; for part 4 as pseudocode down below. I know this isn't the same as having a fully functional unit-tester, but please let me know if this can 
;; count for anything. I put in a ton of time working on this unit tester so I'm pretty bummed that it isn't able to run properly. 

;; PSEUDOCODE:
;; 1. I declared the following global variables:
;; (define passed-points 0)
;; (define total-points 0)
;; (define exercise-list! '())

;; 2. I modified add-my-test in the following way:
;; whenever a test is added to my-tests!, the name of the exericse is also added to exercise-list! IFF that 
;; exercise name has not already been added to the exercise list. I belive I used member? to do this.

;; 3. I modifered run-all-tests such that it iterated through the exercise-list! global variable.