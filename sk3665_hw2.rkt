#lang racket

;Suin Kim CS270-002

;The following two lines are required to test the code.
(require rackunit)
(require rackunit/text-ui)

#|
CS 270 Spring 2019
Homework 2
Content by:  Profs. Bruce Char, Mark Boady, and Jeremy Johnson

Complete each of the below functions.

Tests given are not designed to be comprehensive.
They will give you an idea if your code is right, but they do not test all possible cases.

Think about your design -- when grading, we may add additional tests for your
functions.  So your design must anticipate what needs to be done for complete
correctness, not just reacting to the tests you see originally.

Once you write a function, you may use it in later questions.

Use of loop commands for/while, etc. will result in 0 points for the question.
We'd like to practice using recursion instead.

Use of the built-in and/or/not commands will result in 0 points for the question.
For this homework, we'd like you to demonstrate that you understand
how boolean logic works at the lowest levels, rather than having
something automatic do it for you.


|#

;Question 1 (4 points)
;In these series of problems, we will implement our own versions of
;various symbolic logic operations.  We will follow the general programming
;convention that if the built-in version of the function is called "X"
;our function will be called Xi:  andi, ori, noti, etc.
;Complete the definition of noti to implement logical negation.
;Use only if statements, constants, and comparisons.
;You should not use the built-in logical functions to implement
;your own -- don't use not, or, and, etc.  in your implementation of noti.

;The homework includes the automatic test-case checking functions
; define-test-suite, check-equal?, and run-tests.  Read the racket
; documentation about these built-in functions.  We are using these
; built-in functions to run tests that check correctness
; but we aren't using them to implement noti, andi, etc.


; logical negation
; input:  (boolean? e)
; output:  (boolean? (noti e))
; When e is true return false
; When e is false return true
(define (noti e)
  (if (equal? e #f) ;checks if e is true or false
      #t ;returns true if e is false
      #f ;returns false if e is true
      )
 )

;Tests
(define-test-suite testnoti
  (check-equal?
    (noti #f) #t)
  (check-equal?
    (noti #t) #f)
)
(display "Question 1 noti (4 points)")
(newline)
(run-tests testnoti 'verbose)


;Question 2 (8 points)
;Complete the definition of andi to implement logical and.  
;Use only if statements, constants, and comparisons.

; logical and
; input:  e1 and e2 are boolean expressions
; output:  (boolean? (andi e1 e2))
; True when e1 and e2 are both true
; False otherwise
(define (andi e1 e2)
  (if (equal? e1 #t) ;check if both e1 and e2 are true
      (if (equal? e2 #t)
          #t ;only returns true if both e1 and e2 are true
          #f ;returns false if either or both e1 or e2 are false
          )
      #f
      )
)

;Tests
(define-test-suite testandi
  (check-equal?
    (andi #f #f) #f)
  (check-equal?
    (andi #f #t) #f)
  (check-equal?
    (andi #t #f) #f)
  (check-equal?
    (andi #t #t) #t)
)
(display "Question 2 andi (8 points)")
(newline)
(run-tests testandi 'verbose)


;Question 3 (8 points)
;Complete the definition of ori to implement logical or.
;Use only if statements, constants, and comparisons.

; logical or
; input:  e1 and e2 are boolean expressions
; output:  (boolean? (ori e1 e2))
; return true if either e1 or e2 are true

(define (ori e1 e2)
  (if (equal? e1 #t) ; if e1 is true, return true
      #t
      (if (equal? e2 #t) 
          #t ;if e1 is false, but e2 is true, return true
          #f ;if e1 and e2 are false, return false
          )
      )
)

;Tests
(define-test-suite testori
  (check-equal?
    (ori #f #f) #f)
  (check-equal?
    (ori #f #t) #t)
  (check-equal?
    (ori #t #f) #t)
  (check-equal?
    (ori #t #t) #t)
)
(display "Question 3 ori (8 points)")
(newline)
(run-tests testori 'verbose)


;Question 4 (8 points)
;Complete the definition of xori to implement logical exclusive or.
;Use only if statements, constants, and comparisons.  


; logical xor
; input:  e1 and e2 are boolean expressions
; output:  (boolean? (xori e1 e2))
; return true if exactly one of e1 or e2 are true

(define (xori e1 e2)
  (if (equal? e1 e2) ;if e1 and e2 are both true or both false, returns false 
      #f
      (if (equal? e1 #t) ;check if e1 is true or false
          (if (equal? e2 #t) ;check if e2 is true of false
              #f ;return false if both are true
              #t ;return true if e1 is true and e2 is false
              )
          #t ;return true if e1 is false, e2 is true since e1 and e2 are not the same
          )
      )
)

;Tests
(define-test-suite testxori
  (check-equal?
    (xori #f #f) #f)
  (check-equal?
    (xori #f #t) #t)
  (check-equal?
    (xori #t #f) #t)
  (check-equal?
    (xori #t #t) #f)
)
(display "Question 4 xori (8 points)")
(newline)
(run-tests testxori 'verbose)


;Question 5 (8 points)
;Complete the definition of impliesi to implement logical implication.
;Use only if statements, constants, and comparisons. 


; logical implication
; input:  e1 and e2 are boolean expressions
; output:  (boolean? (impliesi e1 e2))
; return true if e1 is false
; or e1 is true and e2 is true
(define (impliesi e1 e2)
  (if (equal? e1 #f) ;check if e1 is false and returns #t if so
      #t
      (if (equal? e2 #t)
          #t ;returns true if both e1 and e2 are true
          #f
          )
      )
)

;Tests
(define-test-suite testimpliesi
  (check-equal?
    (impliesi #f #f) #t)
  (check-equal?
    (impliesi #f #t) #t)
  (check-equal?
    (impliesi #t #f) #f)
  (check-equal?
    (impliesi #t #t) #t)
)

(display "Question 5 impliesi (8 points)")
(newline)
(run-tests testimpliesi 'verbose)


;Question 6 (8 points)
;Complete the definition of iffi to implement logical equivalence (if and only if).
;Use only if statements, constants, and comparisons.  


; logical iffi
; input:  e1 and e2 are boolean expressions
; output:  (boolean? (iffi e1 e2)) true if e1 and e2 are both true of
;           both false
(define (iffi e1 e2)
  (if (equal? e1 e2) ;checks if both e1 and e2 are both true or false
      #t
      #f
      )
)

;Tests
(define-test-suite testiffi
  (check-equal?
    (iffi #f #f) #t)
  (check-equal?
    (iffi #f #t) #f)
  (check-equal?
    (iffi #t #f) #f)
  (check-equal?
    (iffi #t #t) #t)
)
(display "Question 6 iffi (8 points)")
(newline)
(run-tests testiffi 'verbose)


; Question 7 (4 points)
; Define a unit test to test the Boolean equivalence
; (iffi e1 e2) <-> (andi (impliesi e1 e2) (impliesi e2 e1))
; Note that the symbol <-> means equivalent.
; You should create four test cases for all possible values (#f and #t).
; Prove the two expressions are the same by testing on all 4 possible inputs.

; (iffi e1 e2) <-> (andi (impliesi e1 e2) (impliesi e2 e1))
(define-test-suite testiffequiv
  (check-equal?
   (iffi #t #t) #t)
  (check-equal?
   (iffi #f #f) #t)
  (check-equal?
   (iffi #t #f) #f)
  (check-equal?
   (iffi #f #t) #f)
)

(display "Question 7 iff proof (4 points)")
(newline)
(run-tests testiffequiv 'verbose)


; Question 8 (4 points)
;Define a unit test to test the Boolean equivalence
; (impliesi e1 e2) <-> (ori (noti e1) e2)
; Note that the symbol <-> means equivalent.
; You should create four test cases for all possible values (#f and #t).
; Prove the two expressions are the same by testing on all 4 possible inputs.


; (impliesi e1 e2) <-> (ori (noti e1) e2)
(define-test-suite testimpliesequiv
  (check-equal?
   (impliesi #t #t) #t)
  (check-equal?
   (impliesi #f #f) #t)
  (check-equal?
   (impliesi #t #f) #f)
  (check-equal?
   (impliesi #f #t) #t)
)

(display "Question 8 implies proof (4 points)")
(newline)
(run-tests testimpliesequiv 'verbose)



;Question 9.
; Use foldr to implement (andlist L).
; andlist takes a list and ANDs all the elements in the list.
; You may use your andi command from above.
(define (andlist L)
  (if (null? L) ;test if L is null, return true if so
  #t
  (foldr andi (first L) (rest L)) ;use foldr and andi function to get AND of all elements in L
  )
)

;Tests
(define-test-suite testandlist
  (check-equal?
    (andlist '()) #t)
  (check-equal?
    (andlist '(#t #t #t)) #t)
  (check-equal?
    (andlist '(#t #f #t)) #f)
  (check-equal?
    (andlist '(#t #t #t #t #f)) #f)
)
(display "Question 9 andlist (4 points)")
(newline)
(run-tests testandlist 'verbose)



;Question 10.
; Use foldr to implement (orlist L).
; andlist takes a list and ORs all the elements in the list.
; You may use your ori command from above.
(define (orlist L)
  (if (null? L) ;check if L is null, return false if so
      #f
      (foldr ori (first L) (rest L)) ;use foldr and ori function to get OR of all elements in L
      )
)

;Tests
(define-test-suite testorlist
  (check-equal?
    (orlist '()) #f)
  (check-equal?
    (orlist '(#f #f #f)) #f)
  (check-equal?
    (orlist '(#t #f #t)) #t)
  (check-equal?
    (orlist '(#t #t #t #t #f)) #t)
)
(display "Question 10 orlist (4 points)")
(newline)
(run-tests testorlist 'verbose)

;Question 11 (12 points)
; Write a recursive function allones to check if a list of integers
; contains all ones.
; Don't forget the base case and the necessary recursion.
; You may use any previously written function.


; Check if a list contains all ones
; Input:  L is a list of integers.
; Output: a boolean value which is true when all of the elements in L
;         are equal to one and false otherwise.
; The empty list should return true.
(define (allones L)
    (cond
      [(null? L) #t] ;check if L is null
      [(noti (equal? (first L) 1)) #f] ;check if element is not 1
      [else (allones (rest L))] ;recursive call for next element
      )
)

(define-test-suite testallones
  (check-equal? (allones '()) #t)
  (check-equal? (allones '(1)) #t)
  (check-equal? (allones '(0)) #f)
  (check-equal? (allones '(0 0)) #f)
  (check-equal? (allones '(0 1)) #f)
  (check-equal? (allones '(1 0)) #f)
  (check-equal? (allones '(1 1)) #t)
  (check-equal? (allones '(1 0 1)) #f)
  (check-equal? (allones '(1 1 1)) #t)
  (check-equal? (allones '(1 1 1 1 1 1 1 1)) #t)
  (check-equal? (allones '(1 0 1 1 1 1 1 1)) #f)
  (check-equal? (allones '(1 1 1 1 2 1 1 1)) #f)
)

(display "Question 11 allones (12 points)")
(newline)
(run-tests testallones 'verbose)


;Question 12 (12 points)
; Write a recursive function atleastone1 to check if a list of integers
; contains at least one 1.
; Don't forget the base case and the necessary recursion. 
; You may use any previously written function.


; Check if a list contains atleast one one
; Input:  L is a list of integers.
; Output: a boolean value which is true when atleast one of the elements
;          in L is equal to one and false otherwise.
; An empty list should return false.
(define (atleastone1 L)
  (cond
      [(null? L) #f] ;check if L is null
      [(equal? (first L) 1) #t] ;check if element in L is 1
      [else (atleastone1 (rest L))] ;recursive call for next element
      )
)

(define-test-suite testatleastone
  (check-equal? (atleastone1 '(1)) #t)
  (check-equal? (atleastone1 '(0)) #f)
  (check-equal? (atleastone1 '(0 0)) #f)
  (check-equal? (atleastone1 '(0 1)) #t)
  (check-equal? (atleastone1 '(1 0)) #t)
  (check-equal? (atleastone1 '(1 1)) #t)
  (check-equal? (atleastone1 '(0 2 0)) #f)
  (check-equal? (atleastone1 '(0 2 1)) #t)
  (check-equal? (atleastone1 '(1 0 1)) #t)
  (check-equal? (atleastone1 '(1 1 1)) #t)
  (check-equal? (atleastone1 '(1 0 1 1)) #t)
  (check-equal? (atleastone1 '(0 0 0 0)) #f)
)

(display "Question 12 atleastone1 (12 points)")
(newline)
(run-tests testatleastone 'verbose)


;Question 13 (12 points)
; Write a recursive function exactlyone0 to check if a list of integers
; contains exactly one 1.
; Don't forget the base case and the necessary recursion.
; You may use any previously written function.

; Hint: a helper function similar to question 12 is helpful here.


; Check if a list contains exactly one 0
; Input:  L is a list of integers.
; Output: a boolean value which is true when exactly one of the elements
;          in L is equal to one and false otherwise.
; The empty list should return false.
(define (exactlyone0_helper L) ;helper function
  (noti (member (first L) (rest L) )) ;check if 0 is found in L again
)

(define (exactlyone0 L)
  (if (null? L) ;check if L is null
      #f
      (if (equal? (first L) 0) ;check if element is 0
          (exactlyone0_helper L) ;calls helper function if element is 0 to check that that the rest of the elements are not 0
          (exactlyone0 (rest L)) ;recursive call for next element
          )
      )
  )
      

(define-test-suite testexactlytone0
  (check-equal? (exactlyone0 '(1)) #f)
  (check-equal? (exactlyone0 '(0)) #t)
  (check-equal? (exactlyone0 '(0 0)) #f)
  (check-equal? (exactlyone0 '(0 1)) #t)
  (check-equal? (exactlyone0 '(1 0)) #t)
  (check-equal? (exactlyone0 '(1 1)) #f)
  (check-equal? (exactlyone0 '(0 0 0)) #f)
  (check-equal? (exactlyone0 '(0 0 1)) #f)
  (check-equal? (exactlyone0 '(1 0 2)) #t)
  (check-equal? (exactlyone0 '(1 1 2)) #f)
  (check-equal? (exactlyone0 '(1 0 1 1)) #t)
  (check-equal? (exactlyone0 '(0 0 0 1)) #f)
)

(display "Question 13 exactlyone0 (12 points)")
(newline)
(run-tests testexactlytone0 'verbose)


;Question 14.
; Write a recursive function oddpositives to check if a list of integers
; contains an odd number of numbers that are greater than zero.
; Don't forget the base case and the necessary recursion. 
; You may use any previously written function.

; Check if a list contains an odd number of positive values
; Input:  L is a list of integers.
; Output: a boolean value which is true when an odd number of the elements
;          in L are positive and false otherwise.
; The Null list should return false (since it has 0 odds, and 0 is not an odd number)
(define (oddpositives_helper L) ;helper function that returns number of positive numbers in L
  (if (null? L) ;cehck if L is null
      0 ;returns 0 if L is null
      (if (positive? (first L)) ;checks if element is positive number
          (+ 1 (oddpositives_helper (rest L))) ;increments counter by 1 if element is positive number
          (oddpositives_helper (rest L)) ;recursive call for next element
          )
      )
  )

(define (oddpositives L)
  (odd? (oddpositives_helper L)) ;checks if number of positive numbers in L is odd
  )

(define-test-suite testoddpositives
  (check-equal? (oddpositives '(1)) #t)
  (check-equal? (oddpositives '(0)) #f)
  (check-equal? (oddpositives '(2 0)) #t)
  (check-equal? (oddpositives '(-1 2)) #t)
  (check-equal? (oddpositives '(1 0)) #t)
  (check-equal? (oddpositives '(1 5)) #f)
  (check-equal? (oddpositives '(0 0 0)) #f)
  (check-equal? (oddpositives '(0 -1 5)) #t)
  (check-equal? (oddpositives '(3 1 0)) #f)
  (check-equal? (oddpositives '(1 1 1)) #t)
  (check-equal? (oddpositives '(1000 -1 1)) #f)
  (check-equal? (oddpositives '(1 1 1 1)) #f)
   (check-equal? (oddpositives '(1 1000 -511 511)) #t)
)

(display "Question 14 oddpositives (12 points)")
(newline)
(run-tests testoddpositives 'verbose)