#|
;; Question 1
<binary-string> ::= 0 
                  | 1
                  | 0 <binary-string>
                  | 1 <binary-string>
                  | 01 <binary-string>
                  | 10 <binary-string>

;; Question 2
<binary-string> ::= 0 
                  | 1
                  | 0 <binary-string>
                  | 1 <binary-string>

;; Question 3
<palindrome> ::= ""
               | 0
               | 1
               | (0 <palindrome> 0)
               | (1 <palindrome> 1)

;; Question 4
<numbers> ::= 0
           | 1
           | 2
           | 3
           | 4
           | 5
           | 6
           | 7
           | 8
           | 9

<alpha> ::= | a
            | b
            | c
            | d
            | e
            | f

 <positive> ::= <number>
              | <alpha>
              | + <number> <positive>
              | + <alpha> <positive>

 <negative> ::= <number>
              | <alpha>
              | - <number> <negative>
              | - <alpha> <negative>

<hex> ::= <positive>
        | <negative>

;; Question 5
<args> ::= <ID> 
         | <ID> <args>

<python> ::= def <ID> <args>