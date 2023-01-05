;; LET-interp-starter.scm
;; CSC 370
;; Fall 2022

(load "helpers.scm")

;; ================ Parser Definitions ==================================

;; This defines the translation from the concrete syntax to the abstract syntax.
;; Whenever  you add to or modify the concrete or abstract syntax the specification
;; below must be updated.

(define the-grammar
  '(
     (program                                      ;; <program> ::= 
       (expression)                                ;;   Concrete    <expression>
       a-prog)                                     ;;   Abstract    (a-prog exp)

      (program
       ("def!" identifier "=" expression)
       def-prog)

     (expression                                   ;; <expression> ::= 
       (number)                                    ;;   Concrete     <number> 
       const-exp)                                  ;;   Abstract     (const-exp num)
     
     (expression                                   ;; <expression> ::=
       ("-(" expression "," expression ")")        ;;                -(<exp>,<exp>)
       diff-exp)                                   ;;                (diff-exp exp1 exp2)

     (expression
       ("+(" expression "," expression ")")
       plus-exp)
        
     (expression
       ("*(" expression "," expression ")")
       times-exp)

     (expression
       ("/(" expression "," expression ")")
       div-exp)

     (expression
       ("<(" expression "," expression ")")
       less-than-exp)

     (expression
       ("<=(" expression "," expression ")")
       less-than-or-equal-exp)

     (expression
       ("=(" expression "," expression ")")
       equal-exp)

      (expression
       ("&(" expression "," expression ")")
       and-exp)

      (expression
       ("|(" expression "," expression ")")
       or-exp)
      
      (expression
       ("!(" expression ")")
       not-exp)

     (expression
       ("zero?(" expression ")")
       zero?-exp)

     (expression
       ("if" expression "then" expression "else" expression)
       if-exp)

     (expression
       (identifier)
       var-exp)

     (expression
       ("let" identifier "=" expression "in" expression)
       let-exp)

      (expression 
        ("#true") 
        const-true-exp)
      
      (expression 
        ("#false") 
        const-false-exp)  

    ))

;; Sets up the parser using the above concrete <-> abstract grammars.
;; Defines a function call parse that takes a string in the concrete
;; syntax and returns the corresponding abstract syntax tree. You must
;; have defined the-grammar first.
(load "lex-scan-parse.scm")

;; ==================== Expressed Values ==================================

(define-datatype expval expval?
  (num-val
    (num number?))
  (bool-val
    (b boolean?))
  (unit-val)
  )

(define expval->bool
  (lambda (ev)
    (cases expval ev
      [bool-val (b) b]
      [num-val (num) (if (= num 0) #f #t)]
      [else (raise-exception 'expval->bool "Expressed value is not a Boolean: ~s" ev)])))

(define expval->num
  (lambda (ev)
    (cases expval ev
	   [num-val (num) num]
     [bool-val (b) (if b 1 0)]
	   [else (raise-exception 'expval->num "Expressed value is not a number: ~s" ev)])))

(define expval->string
  (lambda (ev)
    (cases expval ev
      [num-val (num) (number->string num)]
      [bool-val (b) (if b "#true" "#false")]
      [unit-val () "#void"]
	   )))

;; =================== Environment ========================================

(define-datatype environment environment?
  (empty-env)
  (extend-env
   (var symbol?)
   (val expval?)
   (env environment?)))

(define make-init-env
  (lambda []
    (extend-env 'e (num-val 2.71828) (extend-env 'pi (num-val 3.14159) (empty-env)))
  ))

(define apply-env ; Env x Var -> Expval
  (lambda (env target-var)
    (cases environment env
      [extend-env (var val env)
        (if (equal? var target-var)
          val
          (apply-env env target-var))]
      [empty-env () (raise-exception 'apply-env "No binding for ~s" target-var)]
      )))

(define display-env* (lambda (env)
  (cases environment env
    [extend-env (var val env) 
      (display var) (display " = ") (display (expval->string val)) (if (not (equal? env (empty-env))) (display ", ")) (display-env* env)]
    [empty-env () (display "]\n")])
   ))

(define display-env (lambda (env)
  (display "[")
  (display-env* env)))

;; ==================== Evaluater =========================================

(define value-of-prog
  (lambda (prog env)
    (cases program prog
      [a-prog (exp)  (list (value-of-exp exp env) env)]
      [def-prog (var exp) 
        (let [
                [val (value-of-exp exp env)]]
               (list (unit-val) (extend-env var val env))
              )]
      [else (list (raise-exception 'value-of-prog "Abstract syntax case not implemented: ~s" (car prog)) env)])))

(define value-of-exp
  (lambda (exp env)
    (cases expression exp
      [const-exp [num] (num-val num)]
      [const-true-exp [] (bool-val #t)]
      [const-false-exp [] (bool-val #f)]
      [diff-exp [exp1 exp2]
        (let [
               [ev1 (value-of-exp exp1 env)]
               [ev2 (value-of-exp exp2 env)]]
          (num-val (- (expval->num ev1) (expval->num ev2))))]
      [plus-exp [exp1 exp2]
        (let [
               [ev1 (value-of-exp exp1 env)]
               [ev2 (value-of-exp exp2 env)]]
          (num-val (+ (expval->num ev1) (expval->num ev2))))]
      [times-exp [exp1 exp2]
        (let [
               [ev1 (value-of-exp exp1 env)]
               [ev2 (value-of-exp exp2 env)]]
          (num-val (* (expval->num ev1) (expval->num ev2))))]
      [div-exp [exp1 exp2]
        (let [
               [ev1 (value-of-exp exp1 env)]
               [ev2 (value-of-exp exp2 env)]]
          (if (= 0 (expval->num ev2)) (raise-exception 'value-of-exp "Division by 0.")
            (num-val (/ (expval->num ev1) (expval->num ev2)))))]
      [less-than-exp [exp1 exp2]
        (let [
               [ev1 (value-of-exp exp1 env)]
               [ev2 (value-of-exp exp2 env)]]
          (bool-val (< (expval->num ev1) (expval->num ev2))))]
      [less-than-or-equal-exp [exp1 exp2]
        (let [
               [ev1 (value-of-exp exp1 env)]
               [ev2 (value-of-exp exp2 env)]]
          (bool-val (<= (expval->num ev1) (expval->num ev2))))]
      [equal-exp [exp1 exp2]
        (let [
               [ev1 (value-of-exp exp1 env)]
               [ev2 (value-of-exp exp2 env)]]
          (bool-val (= (expval->num ev1) (expval->num ev2))))]
      [and-exp [exp1 exp2]
        (let [
               [ev1 (value-of-exp exp1 env)]
               [ev2 (value-of-exp exp2 env)]]
          (bool-val (and (expval->bool ev1) (expval->bool ev2))))]
      [or-exp [exp1 exp2]
        (let [
               [ev1 (value-of-exp exp1 env)]
               [ev2 (value-of-exp exp2 env)]]
          (bool-val (or (expval->bool ev1) (expval->bool ev2))))]
      [not-exp [exp1]
        (let [
               [ev1 (value-of-exp exp1 env)]]
          (bool-val (not (expval->bool ev1))))]
      [zero?-exp [exp]
        (let [[ev (value-of-exp exp env)]]
          (bool-val (= (expval->num ev) 0)))]
      [if-exp [exp1 exp2 exp3]
        (let [[ev (value-of-exp exp1 env)]]
          (if (expval->bool ev)
            (value-of-exp exp2 env)
            (value-of-exp exp3 env)))]
      [var-exp [var]
        (apply-env env var)]
      [let-exp [var exp body]
        (let [[ev (value-of-exp exp env)]]
          (value-of-exp body (extend-env var ev env)))]
      [else
        (raise-exception
          'value-of-exp
          "Abstract syntax case not implemented: ~s" (car exp))])))


;; =================== Interpreter =========================================

(define !debug1 (lambda ()
  (trace value-of-prog)
  (trace value-of-exp)))

(define !debug2 (lambda ()
  (trace value-of-prog)
  (trace value-of-exp)
  (trace expval->bool)
  (trace expval->num)
  (trace expval->string)))

(define !debug0 (lambda ()
  (untrace value-of-prog)
  (untrace value-of-exp)
  (untrace expval->bool)
  (untrace expval->num)
  (untrace expval->string)))


(define !env (lambda (env)
  (display-env env)))

;; (start) -- Starts the interpreter.
(define start
  (lambda ()
    (begin
      (display "\n")
      (display "██████╗  █████╗ ███╗   ██╗██╗    ███████╗ ██████╗██████╗ ██╗██████╗ ████████╗\n")
      (display "██╔══██╗██╔══██╗████╗  ██║██║    ██╔════╝██╔════╝██╔══██╗██║██╔══██╗╚══██╔══╝\n")
      (display "██║  ██║███████║██╔██╗ ██║██║    ███████╗██║     ██████╔╝██║██████╔╝   ██║   \n")
      (display "██║  ██║██╔══██║██║╚██╗██║██║    ╚════██║██║     ██╔══██╗██║██╔═══╝    ██║   \n")
      (display "██████╔╝██║  ██║██║ ╚████║██║    ███████║╚██████╗██║  ██║██║██║        ██║   \n")
      (display "╚═════╝ ╚═╝  ╚═╝╚═╝  ╚═══╝╚═╝    ╚══════╝ ╚═════╝╚═╝  ╚═╝╚═╝╚═╝        ╚═╝   \n")
      (display "                                                                             \n")
      (read-eval-print (make-init-env)))))

;; (read-eval-print) -- Main read, eval, and print loop.
(define read-eval-print
  (lambda (env)
    ;; Display an interpreter prompt
    (display "==> ")
    ;; Read a line user input
    (let ([concrete-code (get-input-string)])
      (cond
        [(equal? concrete-code "!quit")  
          (display "Goodbye!")  ;; Quit if 'quit entered.
          (newline)]
        [(equal? concrete-code "!debug1") (!debug1) (read-eval-print env)]
        [(equal? concrete-code "!debug2") (!debug2) (read-eval-print env)]
        [(equal? concrete-code "!debug0") (!debug0) (read-eval-print env)]
        [(equal? concrete-code "!env") (!env env) (read-eval-print env)]
        [(equal? concrete-code "!reset-env") (read-eval-print (make-init-env))]
        [else
          (guard
            [ex [else (display "PARSE ERROR: \n") (display-exception ex)]]
            ;; Parse code, eval expression, and print result.
            (let
              ([abstract-code (parse concrete-code)])
              (guard
                [ex [else (display "RUNTIME ERROR: \n") (display-exception ex)]]
                (let [
                  [result (value-of-prog abstract-code env)]]
                  (let [
                    [value (car result)]
                    [cur-env (cadr result)]]
                    (display (expval->string value))
                (newline) (read-eval-print cur-env)))))
          ;; "Loop".  Notice it is tail recursive.
      )]
        ))))