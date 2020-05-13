#lang racket

(require rackunit)
(require rackunit/text-ui)

;Suin Kim CS270-002

;CS 270
;Professors Bruce Char, Mark Boady and Professor Jeremy Johnson
;Drexel University
;Homework 4

;In this exercise, we will directly relate integers and boolean values.
;We will use this ability to generate partial truth tables.
;Specifically, we will generate the inputs to the function.
;Next week, we will create the truth table outputs.
;Afterwards, will will combine these parts to solve a problems.

;Question 1
;Convert an Integer to a List of true/false values.
;If we want to convert the integer 9 to a list of t/f values
;we start by finding out its remainder and quotient when divided by 2.
;(remainder 9 2) = 1
;(quotient 9 2) = 4
;This tells us the least significant bit is a 1
;We will represent 1 as #t, so the list is currently (#t)
;Next, we repeat the proccess with the quotient 4
;(remainder 4 2) = 0
;(quotient 4 2) = 2
;Zero is false, so we add this to the list (#f #t)
;Repeat with the quotient 2
;(remainder 2 2) = 0
;(quotient 2 2) = 1
;Zero is false, the list becomes (#f #f #t)
;(remainder 1 2) = 1
;(quotient 1 2) = 0
;One is true, the list because (#t #f #f #t)
;The quotient was zero meaning we can stop.

;Implement a converter function int_to_bool which takes any non-negative integer n
; and returns a list of #ts and #fs corresponding to the 1's and 0's of the binary representation of n
; Thought question:  why does the result list always begin with #t? 

;It is easiest to make L a parameter and use a recursive helper function.
;int_to_bool is not recursive but it calls int_to_bool_h which is recursive.

(define (int_to_bool n) ; n expected to be a non-negative integer.
  (int_to_bool_h n '())
)
(define (int_to_bool_h n L)
  (cond
    [(zero? n) L] ;stop when n is 0
    [(equal? (remainder n 2) 1) (int_to_bool_h (quotient n 2) (append '(#t) L))] ;check if 1, appends #t to L and recursive call with n/2 and appended L 
    [else (int_to_bool_h (quotient n 2) (append '(#f) L))] ;check if 0, appends #f to L and recursive call with n/2 and appended L
    )
)
;Test to see if you function works correctly
(define-test-suite test_int_to_bool
  (check-equal? (int_to_bool 0) '())
  (check-equal? (int_to_bool 1) '(#t))
  (check-equal? (int_to_bool 2) '(#t #f))
  (check-equal? (int_to_bool 3) '(#t #t))
  (check-equal? (int_to_bool 4) '(#t #f #f))
  (check-equal? (int_to_bool 5) '(#t #f #t))
  (check-equal? (int_to_bool 6) '(#t #t #f))
  (check-equal? (int_to_bool 7) '(#t #t #t))
  (check-equal? (int_to_bool 8) '(#t #f #f #f))
  (check-equal? (int_to_bool 9) '(#t #f #f #t))
  (check-equal? (int_to_bool 10) '(#t #f #t #f))
  (check-equal? (int_to_bool 11) '(#t #f #t #t))
  (check-equal? (int_to_bool 12) '(#t #t #f #f))
  (check-equal? (int_to_bool 13) '(#t #t #f #t))
  (check-equal? (int_to_bool 14) '(#t #t #t #f))
  (check-equal? (int_to_bool 15) '(#t #t #t #t))
)
(display "Question 1.) int_to_bool Results (32 points, 2 per test)\n")
(define q1_grade (- 32 (* 2 (run-tests test_int_to_bool))))


;Question 2
;Only significant binary digits are stored by the above function.
;In reality, we would want every number to have the same bit length.
;Write a function to pad #f onto the front of the list.
;What will your function do if the pad length is less than length of the int_to_bool result?
(define (pad num_bits bit_list)
  (if (<= num_bits (length bit_list)) ;checks if num_bits <= length of bit_list, if true return bit_list
      bit_list
      (pad num_bits (append '(#f) bit_list)) ;recursive call with appended bit_list, length of bit_list is incremented by 1
    )
)
;Check your function with the below tests
(define-test-suite test_pad
  (check-equal? (pad 5 (int_to_bool 0))  '(#f #f #f #f #f))
  (check-equal? (pad 5 (int_to_bool 1))  '(#f #f #f #f #t))
  (check-equal? (pad 5 (int_to_bool 2))  '(#f #f #f #t #f))
  (check-equal? (pad 5 (int_to_bool 3))  '(#f #f #f #t #t))
  (check-equal? (pad 5 (int_to_bool 4))  '(#f #f #t #f #f))
  (check-equal? (pad 5 (int_to_bool 5))  '(#f #f #t #f #t))
  (check-equal? (pad 5 (int_to_bool 6))  '(#f #f #t #t #f))
  (check-equal? (pad 5 (int_to_bool 7))  '(#f #f #t #t #t))
  (check-equal? (pad 5 (int_to_bool 8))  '(#f #t #f #f #f))
  (check-equal? (pad 5 (int_to_bool 9))  '(#f #t #f #f #t))
  (check-equal? (pad 5 (int_to_bool 10)) '(#f #t #f #t #f))
  (check-equal? (pad 5 (int_to_bool 11)) '(#f #t #f #t #t))
  (check-equal? (pad 5 (int_to_bool 12)) '(#f #t #t #f #f))
  (check-equal? (pad 5 (int_to_bool 13)) '(#f #t #t #f #t))
  (check-equal? (pad 5 (int_to_bool 14)) '(#f #t #t #t #f))
  (check-equal? (pad 5 (int_to_bool 35)) '(#t #f #f #f #t #t))
)
(display "Question 2.) pad Results (16 points, 1 per test)\n")
(define q2_grade (- 16 (run-tests test_pad)))

;Question 3
;Generate a Truth Table
;Given a number of variables n
;generate a truth table will all variable settings.
;The truth table should have rows with values starting at
;2^n-1 and ending at 0.
;For example, the truth tables for n=2 is
;( (#t #t) (#t #f) (#f #t) (#f #f) )
;Notice: A "Table" is a list of lists
;As integers this is (3 2 1 0)
;The number of bits is n.

;Define the below function
(define (tt_inputs n)
  (tt_inputs_h n (- (expt 2 n) 1))
)
(define (tt_inputs_h bits row_val)
  (if (zero? row_val) ;stop when row_val is 0
      (cons (pad bits (int_to_bool 0)) '())
      (cons (pad bits (int_to_bool row_val)) (tt_inputs_h bits (- row_val 1))) ;recursive call to generate new row
    )
)
;Check your function with the following tests
(define-test-suite test_tt
  (check-equal? (tt_inputs 0)
                '(())
  )
  (check-equal? (tt_inputs 1)
                '( (#t) (#f) )
  )
  (check-equal? (tt_inputs 2)
                '( (#t #t) (#t #f) (#f #t) (#f #f))
  )
  (check-equal? (tt_inputs 3)
                '( (#t #t #t)
                   (#t #t #f)
                   (#t #f #t)
                   (#t #f #f)
                   (#f #t #t)
                   (#f #t #f)
                   (#f #f #t)
                   (#f #f #f)
                   )
   )
 #|
  (check-equal? (tt_inputs 4)
                '(
                   (#t #t #t #t)
                   (#t #t #t #f)
                   (#t #t #f #t)
                   (#t #t #f #f)
                   (#t #f #t #t)
                   (#t #f #t #f)
                   (#t #f #f #t)
                   (#t #f #f #f)
                   (#f #t #t #t)
                   (#f #t #t #f)
                   (#f #t #f #t)
                   (#f #t #f #f)
                   (#f #f #t #t)
                   (#f #f #t #f)
                   (#f #f #f #t)
                   (#f #f #f #f)
                   )
   )
|#
   (check-equal? (tt_inputs 5)
                '(
                   (#t #t #t #t #t)
                   (#t #t #t #t #f)
                   (#t #t #t #f #t)
                   (#t #t #t #f #f)
                   (#t #t #f #t #t)
                   (#t #t #f #t #f)
                   (#t #t #f #f #t)
                   (#t #t #f #f #f)
                   (#t #f #t #t #t)
                   (#t #f #t #t #f)
                   (#t #f #t #f #t)
                   (#t #f #t #f #f)
                   (#t #f #f #t #t)
                   (#t #f #f #t #f)
                   (#t #f #f #f #t)
                   (#t #f #f #f #f)
                   (#f #t #t #t #t)
                   (#f #t #t #t #f)
                   (#f #t #t #f #t)
                   (#f #t #t #f #f)
                   (#f #t #f #t #t)
                   (#f #t #f #t #f)
                   (#f #t #f #f #t)
                   (#f #t #f #f #f)
                   (#f #f #t #t #t)
                   (#f #f #t #t #f)
                   (#f #f #t #f #t)
                   (#f #f #t #f #f)
                   (#f #f #f #t #t)
                   (#f #f #f #t #f)
                   (#f #f #f #f #t)
                   (#f #f #f #f #f)
                   )
   )
)
(display "Question 3.) tt_inputs Results (20 points, 4 per test)\n")
(define q3_grade (- 20 (* 4 (run-tests test_tt))))

;Question 4
;The inputs we made above have the format '(#t #f #f #t).
;We need boolean expressions that work with this format.
;Example: (a -> b) = (~a \/ b)
;let is a Racket operation like define, that binds a name such as "a" to a value for the body of the let
;expression.  boolean_vars is expected to be a list of two truth values.  The let below
;binds the symbol a to the first value in the list, the symbol b to the second value in the list (i.e. it
;uses zero-indexing.)
(define (implies_verify boolean_vars)
  (let (;Start of name list
        (a (list-ref boolean_vars 0));Pairs (name value)
        (b (list-ref boolean_vars 1))
      );End of name list
    (equal? (implies a b ) (or (not a) b))
 );end of let
)
;Test Implies Def
(define-test-suite test_implies
  (check-equal? (implies_verify '(#t #t)) #t)
  (check-equal? (implies_verify '(#t #f)) #t)
  (check-equal? (implies_verify '(#f #t)) #t)
  (check-equal? (implies_verify '(#f #f)) #t)
)
(display "Example.) Results of Implies Verify\n")
(define test_question (run-tests test_implies))


;Write the following three simple boolean expressions as functions. 

;a.) Demorgan's Law
;implement ~(a /\ b) = (~a \/ ~b)
(define (demorgan_verify bool_vars)
 (let (
       (a (list-ref bool_vars 0))
       (b (list-ref bool_vars 1))
       )
   (equal? (not (and a b)) (or (not a) (not b)))
   )
)
;Test Implies Def
(define-test-suite test_demorgan
  (check-equal? (demorgan_verify '(#t #t)) #t)
  (check-equal? (demorgan_verify '(#t #f)) #t)
  (check-equal? (demorgan_verify '(#f #t)) #t)
  (check-equal? (demorgan_verify '(#f #f)) #t)
)
(display "4a.) Results of Demorgan Verify (8 points, 2 per test)\n")
(define q4a_grade (- 8 (* 2 (run-tests test_demorgan))))

;b.) Absorption
;implement (x /\ (x \/ y)) = x
(define (absorp2_verify bool_vars)
 (let (
       (x (list-ref bool_vars 0))
       (y (list-ref bool_vars 1))
       )
   (equal? (and x (or x y)) x)
   )
)
;Test Implies Def
(define-test-suite test_absorp2
  (check-equal? (absorp2_verify '(#t #t)) #t)
  (check-equal? (absorp2_verify '(#t #f)) #t)
  (check-equal? (absorp2_verify '(#f #t)) #t)
  (check-equal? (absorp2_verify '(#f #f)) #t)
)
(display "4b.) Results of Absorption Verify (8 points, 2 per test)\n")
(define q4b_grade (- 8 (* 2 (run-tests test_absorp2))))

;c.) Associativity
;implement x \/ (y \/ z) = (x \/ y) \/ z
(define (assoc_verify bool_vars)
  (let (
        (x (list-ref bool_vars 0))
        (y (list-ref bool_vars 1))
        (z (list-ref bool_vars 2))
        )
    (equal? (or x (or y z)) (or (or x y) z))
    )
)
;Test Implies Def
(define-test-suite test_assoc
  (check-equal? (assoc_verify '(#t #t #t)) #t)
  (check-equal? (assoc_verify '(#t #t #f)) #t)
  (check-equal? (assoc_verify '(#t #f #t)) #t)
  (check-equal? (assoc_verify '(#t #f #f)) #t)
  (check-equal? (assoc_verify '(#f #t #t)) #t)
  (check-equal? (assoc_verify '(#f #t #f)) #t)
  (check-equal? (assoc_verify '(#f #f #t)) #t)
  (check-equal? (assoc_verify '(#f #f #f)) #t)
)
(display "4c.) Results of Associativity Verify (16 points, 2 per test)\n")
(define q4c_grade (- 16 (* 2 (run-tests test_assoc))))


;Display Score
(display "\n\n******************************\n")
(display "Result for Homework 4\n")
(display "Q1: ")
(display q1_grade)
(display "/32\n")
(display "Q2: ")
(display q2_grade)
(display "/16\n")
(display "Q3: ")
(display q3_grade)
(display "/20\n")
(display "Q4a: ")
(display q4a_grade)
(display "/8\n")
(display "Q4b: ")
(display q4b_grade)
(display "/8\n")
(display "Q4c: ")
(display q4c_grade)
(display "/16\n")
(display "Total Points: ")
(display (+ q1_grade q2_grade q3_grade q4a_grade q4b_grade q4c_grade))
(display "/100\n")

;Question 5
;Write a function that takes
;fun - a function that takes a list of boolean values and returns a boolean
;tt - a truth table (list of lists of T/F values)
;And returns a list of T/F values with results
;For example if fun computes (not a)
;and tt = ( (#t) (#f) )
;Then the return of
;(evaluate_tt fun tt) should be (#f #t)
(define (evaluate_tt fun tt)
  (cond
    [(null? tt) '()] ;stops when tt is null/empty
    [(fun (first tt)) (append '(#t) (evaluate_tt fun (rest tt)))] ;checks if fun with current #t/#f values returns #t, appends #t to recursive call for next #t/#f values 
    [else (append '(#f) (evaluate_tt fun (rest tt)))] ;if fun with current #t/#f values returns #f, appends #f to recursive call for next #t/#f values
    )
)
;Test your function
(define-test-suite test_eval_tt
  (check-equal?
   (evaluate_tt implies_verify '( (#t #t) (#t #f) (#f #t) (#f #f)))
   '(#t #t #t #t)
  )
  (check-equal?
   (evaluate_tt demorgan_verify '( (#t #t) (#t #f) (#f #t) (#f #f)))
   '(#t #t #t #t)
  )
  (check-equal?
   (evaluate_tt absorp2_verify '( (#t #t) (#t #f) (#f #t) (#f #f)))
   '(#t #t #t #t)
  )
  (check-equal?
   (evaluate_tt assoc_verify '( (#t #t #t)
                   (#t #t #f)
                   (#t #f #t)
                   (#t #f #f)
                   (#f #t #t)
                   (#f #t #f)
                   (#f #f #t)
                   (#f #f #f)
                   ))
   '(#t #t #t #t #t #t #t #t)
  )
)
(display "5.) Results of Evaluate on Truth Table (12 points)\n")
(run-tests test_eval_tt)

;Question 6
;Write a function that takes a list of true/false values
;and determines if that list is the resultant column of a tautology.
;A null list is by definition a tautology
(define (tautology result_list)
  (cond
    [(null? result_list) #t] ;checks if result_list is null/empty, if so returns #t
    [(equal? (first result_list) #f) #f] ;check if current element is #f, if so returns #f
    [else (tautology (rest result_list))] ;recursive call until #f is reached or result_list is null/empty
    )
)
;Test your function
(define-test-suite test_taut
  (check-equal? (tautology '()) #t)
  (check-equal? (tautology '(#t)) #t)
  (check-equal? (tautology '(#f)) #f)
  (check-equal? (tautology '(#t #t)) #t)
  (check-equal? (tautology '(#t #f)) #f)
  (check-equal? (tautology '(#f #t)) #f)
  (check-equal? (tautology '(#f #f)) #f)
  (check-equal? (tautology '(#t #t #t #t #t #t #t)) #t)
  (check-equal? (tautology '(#t #t #t #t #t #f #t)) #f)
)
(display "6.) Results of Evaluation on Truth Table (18 points)\n")
(run-tests test_taut)

;Question 7
;Write a function that takes a function and the number of variables it has
;and determines if the function is a tautology
(define (is_taut func n)
  (tautology (evaluate_tt func (tt_inputs n))) ;plug in previous functions to define (is_taut func n)
)

;Test your function
(define-test-suite test_is_taut
  (check-equal? (is_taut implies_verify 2) #t)
  (check-equal? (is_taut demorgan_verify 2) #t)
  (check-equal? (is_taut absorp2_verify 2) #t)
  (check-equal? (is_taut assoc_verify 3) #t)
  (check-equal? (is_taut (lambda (X) (and (first X) (second X))) 2) #f)
  (check-equal? (is_taut (lambda (X) (or (first X) (second X))) 2) #f)
)
(display "7.) Results of Is Tautology Question (12 points)\n")
(run-tests test_is_taut)