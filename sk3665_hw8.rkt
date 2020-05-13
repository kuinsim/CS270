#lang racket
(require rackunit)
(require rackunit/text-ui)
#|
CS 270 Spring 2019
Homework 8
Professors Bruce Char, Mark W. Boady and Jeremy Johnson

Complete each of the below functions.

Tests given are not designed to be comprehensive.
They will give you an idea if your code is write, but they do not test all possible cases.

Think about your design.

When grading, we may add additional tests for your functions.

Once you write you function, use Induction to justify the stated property of your function.
|#

;Question 1a (10 points)
;Write a recursive function to compute
;the sum( 2*x + 1 , x = 1..n) for a given n
;You must write a recursive function.
;If you use any iterative commands (for/loop/sum/etc you will receive a 0)
(define (odd_sum n)
  (if (equal? n 0)
      0
      (+ (+ (* 2 n) 1) (odd_sum (- n 1)))
      )
)

;Test Bed
(display "Question 1a odd_sum Tests (10 points)\n")
(define-test-suite test_odd_sum
  (check-equal? (odd_sum 1) 3)
  (check-equal? (odd_sum 2) 8)
  (check-equal? (odd_sum 3) 15)
  (check-equal? (odd_sum 4) 24)
  (check-equal? (odd_sum 5) 35)
  (check-equal? (odd_sum 6) 48)
  (check-equal? (odd_sum 7) 63)
  (check-equal? (odd_sum 8) 80)
  (check-equal? (odd_sum 9) 99)
  (check-equal? (odd_sum 10) 120)
)
(define q1a_score (- 10 (run-tests test_odd_sum 'verbose)))

;Question 1b (10 points)
;Prove by induction, algebra, and equational reasoning that
;n >=0 -> (odd_sum n) = (n+1)^2 -1

;Give your proof below in comments

;Base Case:
;Smallest value of x is x = 1 since x = 1..n
;1. (odd_sum 1)                                     odd_sum with x = 1
;2. (if (equal? 1 0)0...)                           Definition of odd_sum
;3. (if #f 0...)                                    Evaluation of equal?
;4. (+ (+ (* 2 (1)) 1) (odd_sum (- (1) 1)))         Evaluation of if statement
;5. (+ 3 (odd_sum 0))                               Simplify using algebra
;6. (+ 3 (if (equal? 0 0)0...))                     Definition of odd_sum
;7. (+ 3 (if #t 0))                                 Evaluation of if statement
;8. (+ 3 0)                                         Evaluation of equal?
;9. 3                                               Evaluation of line 8

;Thus, the Theorem 1 holds for the Base Case.

;Inductive Hypothesis:
;Assume that for any n>=0, it is true that
;(odd_sum n) = (n + 1)^2 - 1

;Inductive Proof:
;We show that when the theory holds for n then it must hold for n + 1.
;1. (odd_sum (n + 1))                                  Restate problem with n + 1
;2. (if (equal? (n + 1) 0)0...)                        Definition of odd_sum
;3. (if #f 0...)                                       Evaluation of equal?
;4  (+ (+ (* 2 (n + 1)) 1) (odd_sum (- (n + 1) 1)))    Evaluation of if statement
;5. (+ (+ (* 2 (n + 1)) 1) (odd_sum n))                Simplify using algebra
;6. (+ (+ (* 2 (n + 1)) 1) ((n + 1)^2 - 1))            By Inductive Hypothesis
;7. (2 * (n + 1)) + 1) + ((n + 1)^2 - 1)               Rephrase
;8. 2n + 2 + 1 + n^2 + 2n + 1 - 1                      Expand
;9. (n^2 + 4n + 4) - 1                                 Evaluate line 8
;10. (n + 2)^2 - 1                                     Factor
;11. ((n + 1) + 1)^2 - 1                               Definition of odd_sum with n + 1

;This shows that under the Inductive Hypothesis the claim of the theorem holds.

;Conclusion:
;Both the Base Case and Inductive Case hold, therefore the Theorem 1 holds for all n >= 0.
;Q.E.D

;---------------------------------------------------------------------
;---------------------------------------------------------------------
;---------------------------------------------------------------------
;Question 2 (10 points)
; Write a recursive function evenones? to check if a list of integers
; contains an even number of ones.
; Don't forget the base case and the necessary recursion.
; Your answer should avoid using helper functions -- such a short function
; doesn't need one.

; Check if a list contains an odd number of zeros
; Input:  L is a list of integers which are either zero or one.
; Output: a boolean value which is true when an even number of the elements
;          in L is equal to one and false otherwise.
; 0 is even, so the Null list should return true
(define (evenones? X)
  (cond
    [(null? X) #t]
    [(equal? (first X) '1) (xor #t (evenones? (rest X)))]
    [else (evenones? (rest X))]
    )
)
;Test Bed
(display "Question 2a evenones? Tests (10 points)\n")
(define-test-suite test_even_ones
  (check-equal? (evenones? '()) #t)
  (check-equal? (evenones? '(1)) #f)
  (check-equal? (evenones? '(0)) #t)
  (check-equal? (evenones? '(0 0)) #t)
  (check-equal? (evenones? '(1 0)) #f)
  (check-equal? (evenones? '(1 1)) #t)
  (check-equal? (evenones? '(0 2 1)) #f)
  (check-equal? (evenones? '(1 -1 1)) #t)
  (check-equal? (evenones? '(1 0 1)) #t)
  (check-equal? (evenones? '(0 1 0 1)) #t)
)
(define q2a_score (- 10 (run-tests test_even_ones 'verbose)))
;Question 2b (10 points)
;Prove by induction, algebra, and equational reasoning that
;If L contains an even number of ones then (equal? (evenones? L) #t)
;If L contains an odd number of zeros then (equal? (evenones? L) #f)
;Hint: Use multiple cases, (cons x L) where L has an even/odd number of ones, and also whether x is 1 or not.

;Base Case:
;X is '()
;1. (evenones '())            evenones? with X = '()
;2. (cond [(null? '()) #t])   Definition of evenones?
;3. (cond #t #t)              Evaluation of null?
;4. #t                        Evaluation of cond

;Thus, the Theorem 2 holds for the Base Case.

;Inductive Hypothesis:
;Assume
;If L contains an even number of ones then (equal? (evenones? L) #t)
;If L contains an odd number of ones then (equal? (evenones? L) #f)

;Inductive Proof 1:
;Prove that if L contains an even number of ones then (equal? (evenones? (cons '1 L)) #f)
;1. (evenones? (cons '1 L))                                                            Restate problem with (cons '1 L)
;2. (cond [(equal? (first (cons '1 L)) '1) (xor #t (evenones? (rest (cons '1 L))))])   Definition of evenones?
;3. (cond [(equal? '1 '1) (xor #t (evenones? (rest (cons '1 L))))])                    Evaluation of first
;4. (cond [(equal? '1 '1) (xor #t (evenones? L))])                                     Evaluation of rest
;5. (cond #t (xor #t (evenones? L)))                                                   Evaluation of equal?
;6. (xor #t (evenones? L))                                                             Evaluation of cond
;7. (xor #t #t)                                                                        By Inductive Hypothesis
;8. #f                                                                                 Evaluation of xor

;This shows that under the Inductive Hypothesis the claim of the theorem holds.

;Inductive Proof 2:
;Prove that if L contains an odd number of ones then (equal? (evenones? (cons '1 L)) #t)
;1. (evenones? (cons '1 L))                                                            Restate problem with (cons '1 L)
;2. (cond [(equal? (first (cons '1 L)) '1) (xor #t (evenones? (rest (cons '1 L))))])   Definition of evenones?
;3. (cond [(equal? '1 '1) (xor #t (evenones? (rest (cons '1 L))))])                    Evaluation of first
;4. (cond [(equal? '1 '1) (xor #t (evenones? L))])                                     Evaluation of rest
;5. (cond #t (xor #t (evenones? L)))                                                   Evaluation of equal?
;6. (xor #t (evenones? L))                                                             Evaluation of cond
;7. (xor #t #f)                                                                        By Inductive Hypothesis
;8. #t                                                                                 Evaluation of xor

;This shows that under the Inductive Hypothesis the claim of the theorem holds.

;Inductive Proof 3:
;Prove that if L contains an even number of ones then (equal? (evenones? (cons '0 L)) #t)
;1. (evenones? (cons '0 L))                                                            Restate problem with (cons '0 L)
;2. [else (evenones? (rest (cons '0 L)))]                                              Definition of evenones?
;3. [else (evenones? L)]                                                               Evaluation of rest
;4. (evenones? L)                                                                      Evaluation of else Statement
;5. #t                                                                                 By Inductive Hypothesis

;This shows that under the Inductive Hypothesis the claim of the theorem holds.

;Inductive Proof 4:
;Prove that if L contains an odd number of ones then (equal? (evenones? (cons '0 L)) #f)
;1. (evenones? (cons '0 L))                                                            Restate problem with (cons '0 L)
;2. [else (evenones? (rest (cons '0 L)))]                                              Definition of evenones?
;3. [else (evenones? L)]                                                               Evaluation of rest
;4. (evenones? L)                                                                      Evaluation of else Statement
;5. #f                                                                                 By Inductive Hypothesis

;This shows that under the Inductive Hypothesis the claim of the theorem holds.

;Conclusion:
;The Base Case and Inductive Cases hold, therefore the Theorem 2 holds for L
;Q.E.D

;---------------------------------------------------------------------
;---------------------------------------------------------------------
;---------------------------------------------------------------------
;Question 3 (10 points)

;We recall Peano Arithmetic, defined a few weeks ago when we were discussing
;systems of representing integers -- "rock", "DP1", binary, etc.
;Here is another way of defining the first few integers using the Peano
;ideas.
(define (zero? n)
  (eq? n 'zero))

(define (nat? x)
  (cond
    [(zero? x) #t]
    [(pair? x) (and (eq? (first x) 'succ) (nat? (second x)))]
    [else #f]))

(define (succ n)
  (list 'succ n))

(define (pred n)
  (if (zero? n) 'zero (second n)))

(define zero 'zero)
(define one (succ zero))
(define two (succ one))
(define three (succ two))
(define four (succ three))
(define five (succ four))
(define six (succ five))
(define seven (succ six))
(define eight (succ seven))
(define nine (succ eight))
(define ten (succ nine))

;Q3
;Write a function that converts a peano arithmetic number to
;a standard integer. 
(define (peano_to_int s)
  (if (zero? s)
      0
      (+ 1 (peano_to_int (pred s)))
    )
)
;Test Bed
(display "Question 3a peano_to_int Tests (10 points)\n")
(define-test-suite test_peano_to_int
  (check-equal? (peano_to_int zero) 0)
  (check-equal? (peano_to_int one) 1)
  (check-equal? (peano_to_int two) 2)
  (check-equal? (peano_to_int three) 3)
  (check-equal? (peano_to_int four) 4)
  (check-equal? (peano_to_int five) 5)
  (check-equal? (peano_to_int six) 6)
  (check-equal? (peano_to_int seven) 7)
  (check-equal? (peano_to_int eight) 8)
  (check-equal? (peano_to_int nine) 9)
)
(define q3a_score (- 10 (run-tests test_peano_to_int 'verbose)))

;Question 3b (10 points)
;Prove by induction that
;If X is a list in Peano Arithmetic notation with n succ symbols
;then (peano_to_int X) = n

;Give your proof below in comments

;Base Case:
;s = zero
;1. (peano_to_int zero)          peano_to_int with s = zero
;2. (if (zero? zero)0...)        Definition of peano_to_int
;3. (if #t 0...)                 Evaluation of zero?
;4. 0                            Evaluation of if Statement

;Thus, the Theorem 3 holds for the Base Case.

;Inductive Hypothesis:
;Assume
;(peano_to_int X) = n

;Inductive Proof:
;We show that when the theory holds for X then it must hold for (succ X).
;1. (peano_to_int (succ X))                     Restate problem with (succ X)
;2. (if (zero? (succ X))0...)                   Definition of peano_to_int
;3. (if #f 0...)                                Evaluation of zero?
;4. (+ 1 (peano_to_int (pred (succ X))))        Evaluation of if statement
;5. (+ 1 (peano_to_int X))                      Evaluation of pred
;6. (+ 1 n)                                     By Inductive Hypothesis
;7. n + 1                                       Definition of peano_to_int with (succ X)

;This shows that under the Inductive Hypothesis the claim of the theorem holds.

;Conclusion:
;Both the Base Case and Inductive Case hold, therefore the Theorem 3 holds for X
;Q.E.D

;---------------------------------------------------------------------
;---------------------------------------------------------------------
;---------------------------------------------------------------------
;Write a recursive function that takes a list of true/false values
;and determines if the list contains no true values
;The null list should return true (since it contains no true values)

;This function could be applied to the result of evaluating an
;Expression on a truth table. Like in HW5.
(define (contradictory T)
  (cond
    [(null? T) #t]
    [(equal? (first T) #f) (contradictory (rest T))]
    [else #f]
    )
)
;Test Bed
(display "Question 4a contradictory Tests (10 points)\n")
(define-test-suite test_contradictory
  (check-equal? (contradictory '()) #t)
  (check-equal? (contradictory '(#f)) #t)
  (check-equal? (contradictory '(#t)) #f)
  (check-equal? (contradictory '(#t #t)) #f)
  (check-equal? (contradictory '(#t #f)) #f)
  (check-equal? (contradictory '(#t #t #t #t)) #f)
  (check-equal? (contradictory '(#t #t #f #t)) #f)
  (check-equal? (contradictory '(#t #t #f #f)) #f)
  (check-equal? (contradictory '(#f #f #f #f)) #t)
  (check-equal? (contradictory '(#f #f #f #f #f)) #t)
)
(define q4a_score (- 10 (run-tests test_contradictory 'verbose)))

;Question 4b (10 points)
;Prove by induction
;If the list L contains a #t symbol then (contradictory L) = #f
;If the list L does not contains a #t symbol then (contradictory L) = #t

;Give your proof below in comments

;Base Case:
;T is '()
;1. (contradictory '())          contradictory with T = '()
;2. (cond [(null? '()) #t])      Definition of contradictory
;3. (cond #t #t)                 Evaluation of null?
;4. #t                           Evaluation of cond

;Thus, the Theorem 4 holds for the Base Case.

;Inductive Hypothesis:
;Assume
;If the list L contains a #t symbol then (contradictory L) = #f
;If the list L does not contains a #t symbol then (contradictory L) = #t

;Inductive Proof 1:
;Prove that if the list L does not contain a #t symbol then (contradictory (cons #f L)) = #t
;1. (contradictory (cons #f L))                                                   Restate problem with (cons #t L)
;2. (cond [(equal? (first (cons #f L)) #f) (contradictory (rest (cons #f L)))])   Definition of contradictory
;3. (cond [(equal? #f #f) (contradictory (rest (cons #f L)))])                    Evaluation of first
;4. (cond [(equal? #f #f) (contradictory L)])                                     Evaluation of rest
;5. (cond #t (contradictory L))                                                   Evaluation of equal?
;6. (contradictory L)                                                             Evaluation of cond
;7. #t                                                                            By Inductive Hypothesis

;This shows that under the Inductive Hypothesis the claim of the theorem holds.

;Inductive Proof 2:
;Prove that if the list L does not contain a #t symbol then (contradictory (cons #t L)) = #f
;1. (contradictory (cons #t L))                                                   Restate problem with (cons #t L)
;2. [else #f]                                                                     Definition of contradictory
;3. #f                                                                            Evaluation of else

;This shows that under the Inductive Hypothesis the claim of the theorem holds.

;Conclusion:
;The Base Case and Inductive Cases hold, therefore the Theorem 4 holds for L
;Q.E.D

;---------------------------------------------------------------------
;---------------------------------------------------------------------
;---------------------------------------------------------------------
;Q5a (10 Points)
;Write a recursive function duplicate that takes every element in a list
;and makes a second copy of the item.
;For example if we started with (1 2 3)
;then the duplicated list would be (1 1 2 2 3 3)
(define (duplicate X)
  (if (null? X)
      '()
      (cons (first X) (cons (first X) (duplicate (rest X))))
      )
)
(display "Question 5a duplicate Tests (10 points)\n")
(define-test-suite test_duplicate
  (check-equal? (duplicate '()) '())
  (check-equal? (duplicate '(1)) '(1 1))
  (check-equal? (duplicate '(1 2)) '(1 1 2 2))
  (check-equal? (duplicate '(4 6)) '(4 4 6 6))
  (check-equal? (duplicate '((1) (2 3))) '((1) (1) (2 3) (2 3)))
  (check-equal? (duplicate '(4 5 6)) '(4 4 5 5 6 6))
  (check-equal? (duplicate '(7 8 9 10)) '(7 7 8 8 9 9 10 10))
  (check-equal? (duplicate '(1 2 3 4 5)) '(1 1 2 2 3 3 4 4 5 5))
  (check-equal? (duplicate '(9 9 9)) '(9 9 9 9 9 9))
  (check-equal? (duplicate '(1 4 5 6 4 3 4 5))
                '(1 1 4 4 5 5 6 6 4 4 3 3 4 4 5 5))
)
(define q5a_score (- 10 (run-tests test_duplicate 'verbose)))

;Q5b (10 Points)
;Prove By Induction
;(length (duplicate L)) = (* 2 (length L))

;Give your proof below in comments

;Base Cases:
;X is '()
;1. (length (duplicate '()))                 duplicate with X = '()
;2. (length (if (null? '()))'()...)          Definition of duplicate
;3. (length (if #t '()...)                   Evaluation of null?
;4. (length '())                             Evaluation of if statement
;5. 0                                        Evaluation of length

;1. (* 2 (length '()))                       RHS with X = '()
;2. (* 2 0)                                  Evaluation of length
;3. 0                                        Evaluation of *

;Thus, the Theorem 5 holds for the Base Case.

;Inductive Hypothesis:
;Assume
;(length (duplicate L)) = (* 2 (length L))

;Inductive Proof:
;We show that when the theory holds for L then it must hold for (cons x L)
;1. (length (duplicate (cons x L)))                                                               Restate problem with (cons x L)
;2. (length (cons (first (cons x L)) (cons (first (cons x L)) (duplicate (rest (cons x L))))))    Definition of duplicate
;3. (length (cons x (cons x (duplicate (rest (cons x L))))))                                      Evaluation of first
;4. (length (cons x (cons x (duplicate L))))                                                      Evaluation of rest
;5. (+ 1 (length (cons x (duplicate L))))                                                         Evaluation of (length (cons x))
;6. (+ 1 (+ 1 (length (duplicate L))))                                                            Evaluation of (length (cons x))
;7. (+ 1 (+ 1 (* 2 (length L))))                                                                  By Induction Hypothesis
;8. (* 2 (+ 1 (length L)))                                                                        Simplify using algebra

;1. (* 2 (length (cons x L)))                                                                     RHS with (cons x L)
;2. (* 2 (+ 1 (length L)))                                                                        Evaluation of (length (cons x))

;This shows that under the Inductive Hypothesis the claim of the theorem holds.

;Conclusion:
;The Base Case and Inductive Cases hold, therefore the Theorem 5 holds for L
;Q.E.D

;---------------------------------------------------------------------
;---------------------------------------------------------------------
;---------------------------------------------------------------------
;;;;;;;;;;;;;;Grade Summary;;;;;;;;;;;;;;;;;;;;;;;
(display "------Grade Summary------\n")
(display "Q1a Scored: ")
(display q1a_score)
(display "/10\n")
(display "Q1b Scored: ?/10 (Graded by TA)\n")
(display "Q2a Scored: ")
(display q2a_score)
(display "/10\n")
(display "Q2b Scored: ?/10 (Graded by TA)\n")
(display "Q3a Scored: ")
(display q3a_score)
(display "/10\n")
(display "Q3b Scored: ?/10 (Graded by TA)\n")
(display "Q4a Scored: ")
(display q4a_score)
(display "/10\n")
(display "Q4b Scored: ?/10 (Graded by TA)\n")
(display "Q5a Scored: ")
(display q5a_score)
(display "/10\n")
(display "Q5b Scored: ?/10 (Graded by TA)\n")

(define grand_total (+ q1a_score q2a_score q3a_score q4a_score q5a_score))
(display "\n")
(display "Total: ")
(display grand_total)
(display "/100\n")