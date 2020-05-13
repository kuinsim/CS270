#lang racket
(require rackunit)
(require rackunit/text-ui)
#|
CS 270 - Mathematical Foundations of Computer Science
Drexel University Spring 2018-19
Homework 5

Student name:   Suin Kim

For this homework, you will need to use DrRacket.  Strive to avoid unnecessary code in your implementations.
You should have installed it by now on your machine, and know the basics of using it.
Make sure you have the latest version. 


The goal of this assignment is to think about recursive programming and to
review boolean expressions and evaluation.

We write Racket programs that allow an entry of a Boolean expression using
Racket's syntax, and uses recursive programming to evaluate it.  Since "nested
expressions" are a natural situation for the use of recursion, this is a situation
where it's easier to do recursive programming than to avoid it.

Instructions for using this file:

- open this file in DrRacket as cs270_hw5.rkt

- insert your solutions into this file where indicated (for instance as "'replace-this-with-your-implementation")

- make sure the entire file is accepted by DrRacket. If you don't finish some problems, comment them out. The same is true for any English text that you may add. This file already contains many comments, so you can see what the syntax is.

- Submit your homework through Blackboard (learning.drexel.edu) 

- All function names are given, do not change the names of any functions


We use rackunit package to do unit tests. When you start,
all the tests will be failing. Once you implement the required
functions, the unit tests associated with those functions should
pass. Do not modify the unit test blocks.


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
 Assignment Overview

 The goal of this assignment is to create a boolean evaluator
 and use it to determine if a boolean expression is satisfied or not. 
 An expression is satisified if it is true for a specified setting of 
 variables.  This is different from "satisfiable" which talks about
 whether there is a setting of variables that makes the expression true
 without giving the setting.  To use the two terms,  a SAT solver like MiniSAT
 finds a setting of variables that makes an expression satisfied.  If
 it succeeds, then the expression is deemed satisfiable, otherwise it's
 unsatisfiable.  Determining that the setting of variables makes an expression
 satisfied is usually much easier than determining whether the expression
 is satisfiable without being given any settings.  It's like the difference
 between checking that a purported solution to an equation really solves it,
 and solving the equation in the first place.

 This framework could also be used to determine if it is 
 possible to make a statement true for all settings of 
 variables, i.e. a tautology checker.  The algorithm for checking
 that an expression/formula Phi is a tautology could be either
 by checking that Phi is satisfied for any setting of variables,
 or by checking that ~(Phi) is not satisfiable.

 We will slowly build up all the tools needed by solving a
 series of smaller problems first. You need to do this 
 assignment from top to bottom. Functions are defined to be 
 used later on.

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
 Part 1 Lookup

 If we want to set the values of variables in our boolean
 expressions, we need a way to store variable values, and a way to look up
 stored values subsequently.

 The lookup functions takes a variable name and an environment. 
 It returns the value of the variable is assigned.
 If the variable is not assigned it returns an error.
 Your implementation of lookup must be recursive.

 The environment is a list of pairs.  A pair is a list of two items,
 so the environment is a list of lists.
    (variable_name variable_value)

 Inputs: A variable name and environment (that is, a symbol and a list).
 Outputs: The value of the variable as defined by the environment.
  If the environment has multiple pairs that mention the same variable,
  then the lookup function is free to return the value from any pair

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
 Hints: Here is an example of how to throw an error.
 Copy it into your session and see what it does

 (error 'lookup "Variable Name Not Found")
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
|#

(define (lookup target environment)
  (cond
    [(empty? environment) (error 'lookup "Variable Name Not Found")] ;checks if environment is empty/null
    [(equal? target (first (first environment))) (second (first environment))] ;checks if first element in current list is same as target
    [else (lookup target (rest environment))] ;recursive call for rest of environment
    )
 )

;Test Implementation
(define-test-suite lookup-suite
  ;Check Error is correct
  (check-equal? (with-handlers ([exn:fail? (lambda (exn) 101)]) (lookup 'a '()) ) 101)
  (check-equal? (with-handlers ([exn:fail? (lambda (exn) 204)]) (lookup 'c '((a #t) (b #f))) ) 204)
  ;Check Lookups
  (check-equal? (lookup 'a '( (a #f) (b #t))) #f)
  (check-equal? (lookup 'b '( (a #t) (b #f))) #f)
)
(run-tests lookup-suite 'verbose)
;You can test the error with the next line.
;Make sure to comment it back out so you can complete the rest of the assignment
;(lookup 'c '( (a #t) (b #f)))
#|
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
 Part 2 bool-eval

 Next we need to evaluate a boolean expression given a list of variables
 We will build up an evaluator that can evaluate boolean expressions containing
 Variables, Constants (#t,#f), not, or, and, implies, equiv

 We will start with just variables, constants and "or".
 The new expression you should add is "and".
 Test your evaluator after each new case is added.

 Input: A boolean expression and an environment
 Output: The result of evaluating the expression with the variable values 
          from the environment

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

General Helper Function
The following words cannot be used as variable names
 (or and not implies equiv #t #f)
 We need a test so that the system doesn't treat these as variables
|#
(define (is-reserved-word? word)
  (or (equal? word 'TRUE) (equal? word 'FALSE) (equal? word 'OR) (equal? word 'AND)
      (equal? word 'NOT) (equal? word 'IMPLIES) (equal? word 'EQUIV)))

;Helper Functions for Constants
;is-constant: Returns true if the expression is a constant
(define (is-constant? expression)
  ;This is true when the expression is #t or #f
  ;It is false otherwise
  (or (equal? expression 'TRUE) (equal? expression 'FALSE))
 )

;Tests
(is-constant? 'TRUE) ;true
(is-constant? 'FALSE) ;true
(is-constant? 'OR) ;false

;eval-constant: returns the boolean value of the constant
;Note: we don't need environment here, but for consistency all 
; our eval functions will take environment as their second input
(define (eval-constant expression environment)
  (equal? expression 'TRUE)
)
;Tests
(eval-constant 'TRUE '( (a #t) (b #f))) ;returns #t
(eval-constant 'FALSE '( (a #t) (b #f))) ;returns #f


;Helper Functions for Variables
;is-variable?: Returns true when the expression is a symbol
(define (is-variable? expression)
  (and
   (symbol? expression)
   (not (is-reserved-word? expression))
  )
)
;Tests
(is-variable? 'a)
(is-variable? 'b)
(is-variable? 'TRUE)
(is-variable? 'FALSE)
(is-variable? 'OR)
(is-variable? 'AND)
(is-variable? 'NOT)
(is-variable? 'IMPLIES)
(is-variable? 'EQUIV)
;eval-variable: returns the value associated with a variable
;This is why you wrote the lookup function!
(define (eval-variable variable environment)
  (lookup variable environment)
)

;From Here Down, the functions call each other

;Helper Functions for the Or Statement
;is-or: returns true if the statement is an or
(define (is-OR? expression)
  (and (list? expression) (equal? (length expression) 3)(equal? (first expression) 'OR)) ;checks if operator in expression is OR
)
;Evaluate the or statement
;Here is where it gets interesting
;To evaluate an or statement, we need to evaluate the inputs first
(define (eval-OR expression environment)
  ;Use the built in or to find the actual value
  ;so the or looks like (or something1 something2)
  ; (first expression) the word or
  ; (first (rest expression)) the expression something1
  ; (first (rest (rest expression))) the expression something2
  (or ;built-in
   (bool-eval (first (rest expression)) environment)
   (bool-eval (first (rest (rest expression))) environment)
   )
 )

;You have to implement the remaining definitions
;Uncomment this code as you complete parts. You don't have to do it all at once.
;For example, you can implement and test "and" without "implies" if you just
;leave all the implies code commented out.

;is-and: returns true if the statement is an and
(define (is-AND? expression)
  (and (list? expression) (equal? (length expression) 3)(equal? (first expression) 'AND)) ;checks if operator in expression is AND
 )
;eval-and: evaluate an and statement
(define (eval-AND expression environment)
  (and ;built in
   (bool-eval (first (rest expression)) environment)
   (bool-eval (first (rest (rest expression))) environment)
   )
 )

;is-not: returns true if the statement is a not
(define (is-NOT? expression)
  (and (list? expression) (equal? (length expression) 2) (equal? (first expression) 'NOT)) ;checks if operator in expression is NOT, also checks if length of expression is 2
 )
;eval-not: evaluate a not expression
(define (eval-NOT expression environment)
  (not ;built in
   (bool-eval (first (rest expression)) environment)
   )
 )

;is-implies: returns true if the statement is a implies
(define (is-IMPLIES? expression)
  (and (list? expression) (equal? (length expression) 3) (equal? (first expression) 'IMPLIES)) ;checks if operator in expression is IMPLIES
 )

;eval-implies: evaluate an implies expression
(define (eval-IMPLIES expression environment)
  (implies ;built in
   (bool-eval (first (rest expression)) environment)
   (bool-eval (first (rest (rest expression))) environment)
   )
 )

;is-equiv: returns true if the statement is an iff (logical equivalence)
(define (is-EQUIV? expression)
  (and (list? expression) (equal? (length expression) 3) (equal? (first expression) 'EQUIV)) ;checks if operator in expression is EQUIV
 )

;eval-equiv; evaluate an equiv expression
(define (eval-EQUIV expression environment)
  (equal? ;built in, same as equivalent
   (bool-eval (first (rest expression)) environment)
   (bool-eval (first (rest (rest expression))) environment)
   )
 )

(define (bool-eval expression environment)
  (cond
    [;Case 1 Constants
     (is-constant? expression)
     (eval-constant expression environment)
    ]
    [;Case 2 Variables
     (is-variable? expression)
     (eval-variable expression environment)
    ]
    [;Case 3 OR statements
     (is-OR? expression)
     (eval-OR expression environment)
    ]
    [;Case 4 AND statements
     (is-AND? expression)
     (eval-AND expression environment)
    ]
    [;Case 5 NOT statements
     (is-NOT? expression)
     (eval-NOT expression environment)
    ]
    [;Case 6 IMPLIES statements
     (is-IMPLIES? expression)
     (eval-IMPLIES expression environment)
    ]
    [;Case 7 EQUIV statements
     (is-EQUIV? expression)
     (eval-EQUIV expression environment)
    ]
    [;Else Case
     else
     (error 'bool-eval "Expression given was invalid")
    ]
  )
)
;Uncomment the Sections of tests as you complete each set of definitions

;Test Constants
(define-test-suite bool-eval-suite
(check-equal? 
(bool-eval 'TRUE '( (a #t) (b #f))) ;True
#t)
(check-equal? 
(bool-eval 'FALSE '( (a #t) (b #f))) ;False
#f)
;Test Variables
(check-equal? 
(bool-eval 'a '( (a #t) (b #f))) ;True
#t)
(check-equal? 
(bool-eval 'b '( (a #t) (b #f))) ;False
#f)
;Test OR
(check-equal? 
(bool-eval '(OR b b) '( (a #t) (b #f))) ;True
#f)
(check-equal? 
(bool-eval '(OR a b) '( (a #t) (b #f))) ;True
#t)
(check-equal? 
(bool-eval '(OR b a) '( (a #t) (b #f))) ;True
#t)
(check-equal? 
(bool-eval '(OR b b) '( (a #t) (b #f))) ;False
#f)
;Test NOT
(check-equal? 
(bool-eval '(NOT a) '( (a #t) (b #f))) ;False
#f)
(check-equal? 
(bool-eval '(NOT b) '( (a #t) (b #f))) ;True
#t)
;Test AND
(check-equal? 
(bool-eval '(AND a a) '( (a #t) (b #f))) ;True
#t)
(check-equal? 
(bool-eval '(AND a b) '( (a #t) (b #f))) ;False
#f)
(check-equal? 
(bool-eval '(AND b a) '( (a #t) (b #f))) ;False
#f)
(check-equal? 
(bool-eval '(AND b b) '( (a #t) (b #f))) ;False
#f)
;Test IMPLIES
(check-equal? 
(bool-eval '(IMPLIES a a) '( (a #t) (b #f))) ;True
#t)
(check-equal? 
(bool-eval '(IMPLIES a b) '( (a #t) (b #f))) ;False
#f)
(check-equal? 
(bool-eval '(IMPLIES b a) '( (a #t) (b #f))) ;True
#t)
(check-equal? 
(bool-eval '(IMPLIES b b) '( (a #t) (b #f))) ;True
#t)
;Test EQUIV
(check-equal? 
(bool-eval '(EQUIV a a) '( (a #t) (b #f))) ;True
#t)
(check-equal? 
(bool-eval '(EQUIV a b) '( (a #t) (b #f))) ;False
#f)
(check-equal? 
(bool-eval '(EQUIV b a) '( (a #t) (b #f))) ;False
#f)
(check-equal? 
(bool-eval '(EQUIV b b) '( (a #t) (b #f))) ;True
#t)
(check-equal? 
(bool-eval '(EQUIV a b) '( (a #t) (b #t))) ;True
#t)
)
(run-tests bool-eval-suite 'verbose)

#|
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
 Part 3: get-variables

 If we want to test all setting for variables, then we need to know what
 the variables are

 Inputs: a boolean expression
 Outputs: a list with the variables from the expression
 The output should contain NO duplicate variable names.
 Your implementation must be recursive.  

 Hint: What does append do?
 Hint: What does remove-duplicates do?
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
|#
(define (get-variables expression)
  (cond
    [(empty? expression) null] ;check if expression is empty/null
    [(is-constant? expression) null] ;check if expression is constant
    [(is-variable? expression) (cons expression null)] ;check if expression is variable(s)
    [(not (list? expression)) null] ;if previous conditions don't apply, checks if expression is not a list
    [else (remove-duplicates (append (get-variables (first expression)) (get-variables (rest expression))))] ;removes duplicate variables in each recursive call to append variable
    )
 )


(define-test-suite get-variables-suite
(check-equal? 
(get-variables 'a); returns (a)
'(a))

(check-equal? 
(get-variables '(OR a b)); returns the list (a b)
'(a b))

(check-equal? 
(get-variables '(AND (OR a (NOT b)) (IMPLIES c (EQUIV d TRUE)))); returns the list (a b c d)
'(a b c d))

(check-equal? 
(get-variables '(AND (NOT a) a)); returns (a)
'(a))

(check-true 
(or
  (equal? (get-variables '(AND (OR a b) (AND b a)));returns (b a) (Either one of these is correct)
          '(b a))
  (equal? (get-variables '(AND (OR a b) (AND b a)));returns (a b) (Either one of these is correct)
          '(a b))
))
;(b a) or (a b) is correct, but you will find (b a) is the 
;easier approach.

)
(run-tests get-variables-suite 'verbose)
#|
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
 Part 4: make-truth-table

 Given a list of variables give the truth table with all settings of varaibles

 Input: A list of variables
 Output: A list of environments with all possible settings for the variables
 Your implementation must be recursive.

Hint: What does the list command do? For example (list 1 2 3)
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

This sounds easy, but having the output look right is actually a little difficult
We will make some helper functions to get us there

Variable Bindings
A binding for a symbol is a connection between that symbol and a value.
  It is sort of like the concept of "assignment" although as we will see
  we can have a program consider a lot of alternative  bindings for a symbol
  without actually changing any assigned values.

For any variable, it can be either true or false
 Given a variable, we want to create a collection of bindings with the possibilities
 ( ( (var_name_1 value_1) ) ( (var_name_1 value_2) ) )
This seems like it has a lot of nested lists, but we will need them all.
At the top-most point we returns a list of environments -- a list of lists of pairs.
 (make_bindings 'a) returns (environments for a)
 (environments for a) = ( (environment where a=t) (environment where a=nil))
 = ( ((a t)) ((b t)))
 To figure out more about what is meant, look at the tests below that
 are another way of learning what is desired.
|#
(define (make_bindings name)
  (list (list (list name '#t)) (list (list name '#f))) ;use list command to make nested lists
 )

(define-test-suite make-bindings-suite
(check-equal? 
(make_bindings 'a) ; returns (((a #t)) ((a #f))) 
'(((a #t)) ((a #f))) 
)

(check-equal? 
(make_bindings 'c) ; returns (((c #t)) ((c #f))) 
'(((c #t)) ((c #f))) 
)
)
(run-tests make-bindings-suite 'verbose)

;Insert Binding
; Given a binding and a list of environments, 
; Append the binding to the beginning of each environment
; (insert_binding (X) ( (a b c) (m p q) ...))
; returns 
; ( (X a b c) (X m p q) ...)
; Inserting into the empty list should return the empty list
(define (insert_binding binding environments)
  ;We are going through all the environments
  (if (empty? environments) ;check if environments is empty/null
    null
    (cons (append binding (first environments)) (insert_binding binding (rest environments))) ;insert binding to recursive call to repeat
    )
 ) 

(define-test-suite insert-bindings-suite
(check-equal? 
(insert_binding '((a #t)) '()) ; ( )
'()
)

(check-equal? 
(insert_binding '((pears #t)) '( ((grapes #f)) )) ; ( ((pears #t) (grapes #f)) )
'( ((pears #t) (grapes #f)) ) 
)

(check-equal? 
(insert_binding '((a #t)) '( ((b #t)) ((b #f)) )) ;( ((a #t) (b #t)) ((a #t) (b #f)) )
'( ((a #t) (b #t)) ((a #t) (b #f)) ))
)
(run-tests insert-bindings-suite 'verbose)
#|
Insert Multiple Bindings
 When we are extending the truth table, we are adding more then one binding at a time
 Next, we define a function that takes a list of bindings and addes then to the environment.
 Case 1: When the environment is null, return the bindings
 Case 2: When the bindings are null, return null
 Case 3: Neither bindings nor environment are null
 
 This is extending the truth table. Lets look at an example for 2 variables
 The empty table look like ()
 The table with just b looks like
 ( ; start of table
   ( (b #t) ) ;First row has one column
   ( (b #f) ) ;Second row has one column
 ) ; end of table
 This is the same as inserting the bindings of b into the null environment
 The table for a and b looks like
 (;start of table
   ( (a #t) (b #t)) ;Row 1
   ( (a #t) (b #f)) ;Row 2
   ( (a #f) (b #t)) ;Row 3
   ( (a #f) (b #f)) ;Row 4
); End of table
 The rows 1-2 are inserting the first element in the binding into each row in the previous table
 Then we need to append this to the next value in each row of the previous table (rows 3-4)
Like the prior problems, your implementation should be recursive.  At this point we hope you've caught in
that we want all implementations to be recursive unless we say that it's not wanted.
|#
(define (insert_multiple_bindings bindings environments)
  (cond
    [(empty? bindings) null] ;case 1
    [(empty? environments) bindings] ;case 2
    [else (append (insert_binding (first bindings) environments) (insert_multiple_bindings (rest bindings) environments))] ;case 3
    )
 )
(define-test-suite insert-multiple-bindings-suite
(check-equal? 
 (insert_multiple_bindings '( ((a #t)) ((a #f))) '())
 '( ((a #t)) ((a #f)))
)

(check-equal? 
 (insert_multiple_bindings '() '( ((b #t)) ((b #f))))
 '()
)

(check-equal? 
(insert_multiple_bindings '( ((a #t)) ((a #f))) '( ((b #t)) ((b #f))) )
'( 
  ((a #t) (b #t))
  ((a #t) (b #f))
  ((a #f) (b #t))
  ((a #f) (b #f))
)
);end of check
)
(run-tests insert-multiple-bindings-suite 'verbose)


;Extend Table
;Given a table of truth values, extend it by adding a new variable name.
;(extend_table var_name current_table)
;Use the functions you have previously defined.

(define (extend_table var_name current_table)
  (insert_multiple_bindings (make_bindings var_name) current_table) ;combination of previous functions
 )

(define-test-suite extend-table-suite
(check-equal? 
(extend_table 'a '()) 
'( (( a #t)) ((a #f)) )
)

(check-equal?  (extend_table 'a '( (( b #t)) ((b #f)) ))
'( 
  ((a #t) (b #t))
  ((a #t) (b #f))
  ((a #f) (b #t))
  ((a #f) (b #f))
 )
)
)
(run-tests extend-table-suite 'verbose)

;Gen Truth Table
; Given a list of variables generate the truth table for them

(define (make-truth-table var_names)
  (if (empty? var_names) ;checks if var_names is empty/null
    null
    (extend_table (first var_names) (make-truth-table (rest var_names))) ;extends table by 1 for each recursive call
    )
 )

(define-test-suite make-truth-table-suite

(check-equal? 
(make-truth-table '(a));returns ( ((a #t)) ((a #f)))
'( ((a #t)) ((a #f))))

(check-equal? 
(make-truth-table '(a b))
;Result with pretty spacing
;( 
;      ( ( a #t) ( b #t))
;      ( ( a #t) ( b #f))
;      ( ( a #f) ( b #t))
;      ( ( a #f) ( b #f))
;)
'( ((a #t) (b #t)) ((a #t) (b #f)) ((a #f) (b #t)) ((a #f) (b #f)))
);end of check

;This answer is very long (2^4 possibilities)
(check-equal? 
(make-truth-table '(a b c d)) 
'(
    ((a #t) (b #t) (c #t) (d #t))
    ((a #t) (b #t) (c #t) (d #f))
    ((a #t) (b #t) (c #f) (d #t))
    ((a #t) (b #t) (c #f) (d #f))
    ((a #t) (b #f) (c #t) (d #t))
    ((a #t) (b #f) (c #t) (d #f))
    ((a #t) (b #f) (c #f) (d #t))
    ((a #t) (b #f) (c #f) (d #f))

    ((a #f) (b #t) (c #t) (d #t))
    ((a #f) (b #t) (c #t) (d #f))
    ((a #f) (b #t) (c #f) (d #t))
    ((a #f) (b #t) (c #f) (d #f))
    ((a #f) (b #f) (c #t) (d #t))
    ((a #f) (b #f) (c #t) (d #f))
    ((a #f) (b #f) (c #f) (d #t))
    ((a #f) (b #f) (c #f) (d #f))
))
)
(run-tests make-truth-table-suite 'verbose)
#|
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
 Part 5: is_satisfied

 Given a boolean expression, decide if it is satisfied (true for atleast
                             one assignment of the variables)

 Input: A boolean expression with variables
 Output: True if the expression is satisfied, false otherwise
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
A few more helper functions
Given an expression and a truth table
Run the expression on each item and make a list of the results
|#
(define (run-on-truth-table expression tt)
  (if (empty? tt) ;checks if tt is empty/null
    null
    (cons (bool-eval expression (first tt)) (run-on-truth-table expression (rest tt))) ;cons of bool-eval of current row with recursive call on rest of rows
    )
 )

;Tests
(define-test-suite run-on-truth-table-suite

(check-equal? 
(run-on-truth-table '(NOT a) (make-truth-table '(a))) ;returns (#f #t)
'(#f #t))

(check-equal? 
(run-on-truth-table '(OR (NOT a) a) (make-truth-table '(a))); returns (#t #t)
'(#t #t))

(check-equal? 
(run-on-truth-table '(AND a b) (make-truth-table '(a b)));returns (#t #f #f #f)
'(#t #f #f #f))
)
(run-tests run-on-truth-table-suite 'verbose)

;Now you have a list with the results of all your tests
;we need to decide if at least one is true
;Given a list of boolean values, return true if at least one is true 
; Return false otherwise
(define (atleast-one-true my_list)
  (if (empty? my_list) ;checks if my_list is empty/null
    #f
    (or (first my_list) (atleast-one-true (rest my_list))) ;checks  if current element in my__list is true, then recursive call for rest of my_list
  )
)

;Tests
(define-test-suite atleast-one-true-suite

(check-equal? 
(atleast-one-true '());returns #f
#f)

(check-equal? 
(atleast-one-true '(#t));returns #t
#t)

(check-equal? 
(atleast-one-true '(#f #f #f)); returns #f
#f)

(check-equal? 
(atleast-one-true '(#t #f #t)); returns #t
#t)

(check-equal? 
(atleast-one-true '(#f #f #t)); returns #t
#t)
)
(run-tests atleast-one-true-suite 'verbose)

;Finally, determine if the expression is satisfied using all the components
; you have built up
(define (is-satisfied? bool-expression)
  (atleast-one-true (run-on-truth-table bool-expression (make-truth-table (get-variables bool-expression)))) ;combination of previous functions
 )

;Test Case
(define-test-suite is-satisfied-suite

(check-equal? 
(is-satisfied? '(OR (NOT a) a)) ;returns true
#t)

(check-equal? 
(is-satisfied? '(AND (NOT a) a)) ;returns false
#f)

(check-equal? 
 (is-satisfied? 
  '(NOT (EQUIV (IMPLIES a b) (OR (NOT a) b)))
  );returns false
#f)

(check-equal?
  (is-satisfied?
   '(AND (OR p (OR q (NOT p))) (OR (NOT p) (OR q r)))
  ); returns true
 #t)

(check-equal?
  (is-satisfied?
   '(AND (OR p (OR q (NOT r))) (OR (NOT p) (OR q r)))
  ); returns true
 #t)

(check-equal?
  (is-satisfied?
   '(AND (OR p q) (AND (NOT p) (NOT q)))
  ); returns false
 #f)
  
(check-equal?
  (is-satisfied?
   '(AND (OR p q) (AND (OR (NOT p) (NOT q)) (AND (OR p (NOT q))
                                                 (OR (NOT p) q))))
  ); returns false
 #f)
)
(run-tests is-satisfied-suite 'verbose)


;;----------------------------------------------------------------------
;;-------------Grading Results ----------------------------------------
;;----------------------------------------------------------------------
(display "\nHomework Grade\n")
;#Q01 Question 1
;Note: There should be an error for variable not found at the end
(display "Q01 Question 1: Lookup (4 Points)\n")
(define-test-suite q1_test_suite
  (check-equal? (lookup 'a '( ( a #t) (b #f))) #t)
  (check-equal? (lookup 'b '( ( a #t) (b #f))) #f)
  (check-equal? (lookup 'c '( (a #t) (b #t) (c #t) )) #t)
  (check-equal? (lookup 'd '( (a #t) (d #f) (c #f) (b #t))) #f)
)
(define q1_points (- 4 (run-tests q1_test_suite 'verbose)))

;;Part 2
;;#Q02 Question 2
(display "Q02 Question 2: bool-eval supports AND (4 Points)\n")

(define-test-suite q2_test_suite
  (check-equal? (bool-eval '(AND a c) '( (a #t) (b #f) (c #t) (d #f))) #t)
  (check-equal? (bool-eval '(AND a d) '( (a #t) (b #f) (c #t) (d #f))) #f)
  (check-equal? (bool-eval '(AND b c) '( (a #t) (b #f) (c #t) (d #f))) #f)
  (check-equal? (bool-eval '(AND b d) '( (a #t) (b #f) (c #t) (d #f))) #f)
)
(define q2_points (- 4 (run-tests q2_test_suite 'verbose)))

;;#Q03 Question 3
(display "Q03 Question 3: bool-eval supports NOT (2 Points)\n")

(define-test-suite q3_test_suite
  (check-equal? (bool-eval '(NOT a) '( (a #t) (b #f) (c #t) (d #f))) #f)
  (check-equal? (bool-eval '(NOT b) '( (a #t) (b #f) (c #t) (d #f))) #t)
)
(define q3_points (- 2 (run-tests q3_test_suite 'verbose)))

;;#Q04 Question 4
(display "Q04 Question 4: bool-eval supports IMPLIES (4 Points)\n")

(define-test-suite q4_test_suite
  (check-equal? (bool-eval '(IMPLIES a c) '( (a #t) (b #f) (c #t) (d #f))) #t)
  (check-equal? (bool-eval '(IMPLIES a d) '( (a #t) (b #f) (c #t) (d #f))) #f)
  (check-equal? (bool-eval '(IMPLIES b c) '( (a #t) (b #f) (c #t) (d #f))) #t)
  (check-equal? (bool-eval '(IMPLIES b d) '( (a #t) (b #f) (c #t) (d #f))) #t)
)
(define q4_points (- 4 (run-tests q4_test_suite 'verbose)))

;;#Q05 Question 5
(display "Q05 Question 5: bool-eval supports EQUIV (4 Points)\n")
(define-test-suite q5_test_suite
  (check-equal? (bool-eval '(EQUIV a c) '( (a #t) (b #f) (c #t) (d #f))) #t)
  (check-equal? (bool-eval '(EQUIV a d) '( (a #t) (b #f) (c #t) (d #f))) #f)
  (check-equal? (bool-eval '(EQUIV b c) '( (a #t) (b #f) (c #t) (d #f))) #f)
  (check-equal? (bool-eval '(EQUIV b d) '( (a #t) (b #f) (c #t) (d #f))) #t)
)
(define q5_points (- 4 (run-tests q5_test_suite)))

;;Part 3
;;#Q06 Question 6
(display "Q06 Question 6: get_variables (4 Points)\n")
(define-test-suite q6_test_suite
  (check-equal? (get-variables 'a) '(a))
  (check-equal? (get-variables '(OR a b)) '(a b))
  (check-equal? (get-variables '(AND (OR a (NOT b)) (IMPLIES c (EQUIV d TRUE)))) '(a b c d))
  (check-equal? (get-variables '(AND (NOT a) a)) '(a))
)
(define q6_points (- 4 (run-tests q6_test_suite)))

;;Part 4
;;#Q07 Question 7
(display "Q07 Question 7: make_bindings (4 Points)\n")
(define-test-suite q7_test_suite
  (check-equal? (make_bindings 'a) '(((a #t)) ((a #f))))
  (check-equal? (make_bindings 'b) '(((b #t)) ((b #f))))
  (check-equal? (make_bindings 'cake) '(((cake #t)) ((cake #f))))
  (check-equal? (make_bindings 'orange) '(((orange #t)) ((orange #f))))
)
(define q7_points (- 4 (run-tests q7_test_suite)))

;;#Q08 Question 8
(display "Q08 Question 8: insert_binding (3 Points)\n")
(define-test-suite q8_test_suite
  (check-equal? (insert_binding '((a #t)) '()) '() )
  (check-equal? (insert_binding '((robot #t)) '( ((grapes #f)) )) '( ((robot #t) (grapes #f)) ) )
  (check-equal? (insert_binding '((a #t)) '( ((b #t)) ((b #f)) )) '( ((a #t) (b #t)) ((a #t) (b #f)) ))
)
(define q8_points (- 3 (run-tests q8_test_suite)))

;;#Q09 Question 9
(display "Q09 Question 9: insert_multiple_bindings (4 Points)\n")
(define-test-suite q9_test_suite
  (check-equal? (insert_multiple_bindings '( ((a #t)) ((a #f))) '()) '( ((a #t)) ((a #f))))
  (check-equal? (insert_multiple_bindings '() '( ((b #t)) ((b #f)))) '())
  (check-equal? (insert_multiple_bindings '( ((a #t)) ((a #f))) '( ((b #t)) ((b #f))) )
                '(
                  ((a #t) (b #t))
                  ((a #t) (b #f))
                  ((a #f) (b #t))
                  ((a #f) (b #f))
                  )
                )
  (check-equal? (insert_multiple_bindings '( ((c #t)) ((c #f)))
'(((a #t) (b #t))((a #t) (b #f))((a #f) (b #t))((a #f) (b #f)))
)
'(
((c #t) (a #t) (b #t))
((c #t) (a #t) (b #f))
((c #t) (a #f) (b #t))
((c #t) (a #f) (b #f))
((c #f) (a #t) (b #t))
((c #f) (a #t) (b #f))
((c #f) (a #f) (b #t))
((c #f) (a #f) (b #f))
)
)

) 
(define q9_points (- 4 (run-tests q9_test_suite)))

;;#Q10 Question 10
(display "Q10 Question 10: extend_table (3 Points)\n")
(define-test-suite q10_test_suite
  (check-equal? (extend_table 'a '()) '( (( a #t)) ((a #f)) ))
  (check-equal? (extend_table 'a '( (( b #t)) ((b #f)) ))
'(
((a #t) (b #t))
((a #t) (b #f))
((a #f) (b #t))
((a #f) (b #f))
)
)
  (check-equal? (extend_table 'c '(
((a #t) (b #t))
((a #t) (b #f))
((a #f) (b #t))
((a #f) (b #f))
)
)
'(
((c #t) (a #t) (b #t))
((c #t) (a #t) (b #f))
((c #t) (a #f) (b #t))
((c #t) (a #f) (b #f))
((c #f) (a #t) (b #t))
((c #f) (a #t) (b #f))
((c #f) (a #f) (b #t))
((c #f) (a #f) (b #f))
)
)
)
(define q10_points (- 3 (run-tests q10_test_suite)))

;;Q11 Question 11
(display "Q11 Question 11: make-truth-table (3 Points)\n")
(define-test-suite q11_test_suite
  (check-equal? (make-truth-table '(a));returns ( ((a #t)) ((a #f)))
'( ((a #t)) ((a #f))))
  (check-equal? (make-truth-table '(a b))
;Result with pretty spacing
;(
; ( ( a #t) ( b #t))
; ( ( a #t) ( b #f))
; ( ( a #f) ( b #t))
; ( ( a #f) ( b #f))
;)
'( ((a #t) (b #t)) ((a #t) (b #f)) ((a #f) (b #t)) ((a #f) (b #f)))
)
  (check-equal? (make-truth-table '(a b c d))
'(
((a #t) (b #t) (c #t) (d #t))
((a #t) (b #t) (c #t) (d #f))
((a #t) (b #t) (c #f) (d #t))
((a #t) (b #t) (c #f) (d #f))
((a #t) (b #f) (c #t) (d #t))
((a #t) (b #f) (c #t) (d #f))
((a #t) (b #f) (c #f) (d #t))
((a #t) (b #f) (c #f) (d #f))

((a #f) (b #t) (c #t) (d #t))
((a #f) (b #t) (c #t) (d #f))
((a #f) (b #t) (c #f) (d #t))
((a #f) (b #t) (c #f) (d #f))
((a #f) (b #f) (c #t) (d #t))
((a #f) (b #f) (c #t) (d #f))
((a #f) (b #f) (c #f) (d #t))
((a #f) (b #f) (c #f) (d #f))
))

)
(define q11_points (- 3 (run-tests q11_test_suite)))

;;Part 5
;;#Q12 Question 12
(display "Q12 Question 12: run-on-truth-table (3 Points)\n")
(define-test-suite q12_test_suite
  (check-equal? (run-on-truth-table '(NOT a) (make-truth-table '(a))) ;returns (#f #t)
'(#f #t))
  (check-equal? (run-on-truth-table '(OR (NOT a) a) (make-truth-table '(a))); returns (t #t)
'(#t #t))
  (check-equal? (run-on-truth-table '(AND a b) (make-truth-table '(a b)));returns (t #f #f #f)
'(#t #f #f #f))
)
(define q12_points (- 3 (run-tests q12_test_suite)))

;;#Q13 Question 13
(display "Q13 Question 13: atleast-one-true (5 Points)\n")
(define-test-suite q13_test_suite
  (check-equal? (atleast-one-true '()) #f)
  (check-equal? (atleast-one-true '(#t)) #t)
  (check-equal? (atleast-one-true '(#f #t #f)) #t)
  (check-equal? (atleast-one-true '(#f #f #f #f #f)) #f)
  (check-equal? (atleast-one-true '(#f #f #f)) #f)
)
(define q13_points (- 5 (run-tests q13_test_suite)))

;;#Q14 Question 14
(display "Q14 Question 14: is-satisfied? (5 Points)\n")
(define-test-suite q14_test_suite
  (check-equal? (is-satisfied? '(OR (NOT a) a)) ;returns true
#t)

  (check-equal? (is-satisfied?
'(EQUIV (AND a b) (NOT (OR (NOT a) (NOT b))))
);returns true
#t)
  (check-equal? (is-satisfied?
'(EQUIV (OR x y) (NOT (AND (NOT x) (NOT y))))
);Returns true
#t)
  (check-equal? (is-satisfied?
'(EQUIV (AND m n) (OR m n))
)
#t)
  (check-equal? (is-satisfied? '(AND (NOT a) a)) #f)
)
(define q14_points (- 5 (run-tests q14_test_suite)))
;Total
(define total_points (+
          q1_points
          q2_points
          q3_points
          q4_points
          q5_points
          q6_points
          q7_points
          q8_points
          q9_points
          q10_points
          q11_points
          q12_points
          q13_points
          q14_points))
(display "Q1: ")(display q1_points)(display "/4\n")
(display "Q2: ")(display q2_points)(display "/4\n")
(display "Q3: ")(display q3_points)(display "/2\n")
(display "Q4: ")(display q4_points)(display "/4\n")
(display "Q5: ")(display q5_points)(display "/4\n")
(display "Q6: ")(display q6_points)(display "/4\n")
(display "Q7: ")(display q7_points)(display "/4\n")
(display "Q8: ")(display q8_points)(display "/3\n")
(display "Q9: ")(display q9_points)(display "/4\n")
(display "Q10: ")(display q10_points)(display "/3\n")
(display "Q11: ")(display q11_points)(display "/3\n")
(display "Q12: ")(display q12_points)(display "/3\n")
(display "Q13: ")(display q13_points)(display "/5\n")
(display "Q14: ")(display q14_points)(display "/5\n")
(display "Total: ")(display total_points)(display "/52\n")
(display "Precent: ")(display (exact->inexact (* 100 (/ total_points 52))))(display "%\n")