;;; baraci17@ku.edu.tr    Tue Oct  3 15:14:01 2017
;;;  		   	     
;;; Comp200 Project 1  		   	     
;;;  		   	     
;;; Due Oct ?, 2017  		   	     
;;;  		   	     
;;; Before you start:  		   	     
;;;  		   	     
;;; * Please read the detailed instructions for this project from the
;;; file project1.pdf available in the Assignments section of the
;;; course website.  		   	     
;;;  		   	     
;;; * Please read "Project Submission Instructions" carefully and make
;;; sure you understand everything before you start working on your
;;; project in order to avoid problems.
;;;  		   	     
;;; While you are working:  		   	     
;;; * Type all your work and notes in the appropriate sections of this file.
;;; * Please do not delete any of the existing lines.
;;; * Use the procedure names given in the instructions.
;;; * Use semicolon (;) to comment out text, tests and unused code.
;;; * Remember to document your code.
;;; * Remember our collaboration policy: you can discuss with your friends but
;;;  		   	     
;;;   *** NOTHING WRITTEN GETS EXCHANGED ***
;;;  		   	     
;;; When you are done:  		   	     
;;; * Perform a final save and check-in.
;;; * Please do not make any modifications after midnight on the due date.
;;; * Make sure your file loads without errors.
  		   	     
; Ignore the following line. It is necessary
; so the file loads without errors initially:
(define your-answer-here #f)
(#%require (only racket/base random))

;;;;;;;::;;;:;::;;;;::::;;:;::;;;;:::;;;::::;:;;:;::;;;:;::;:::;::;;;:;::;:;:;::;;::
;;; Problem 1: Some Simple Probability Theory
  		   	     
; Description for factorial: (before the definition of each procedure,
; please write a description about what the procedure does and what
; its input and output should be, making sure the lines are commented
; out with semi-colons)  		   	     
  		   	     
(define factorial  		   	     
  (lambda (n)  		   	     
    (if (= n 0)
        1
        (* n (factorial (- n 1))))))

; Test cases for factorial: (after the definition of each procedure,
; please cut and paste some test cases you have run, making sure the
; lines are commented out with semi-colons)
  		   	     
;(factorial 5)  ; -> 120		   	     
;(factorial 7)  ; -> 5040	   	     
;(factorial 10)  ; -> 3628800	   	     
;(factorial 1)  ; -> 1
;(factorial 0)  ; -> 1
  		   	     
;;; Description for binomial:  		   	     
  		   	     
(define binomial  		   	     
  (lambda (n b)
    (/ (factorial n) (* (factorial (- n b)) (factorial b)))))
  		   	     
; Test cases for binomial:  		   	     
  		   	     
;(binomial 5 1)  ; -> 5  		   	     
;(binomial 5 2)  ; -> 10		   	     
;(binomial 10 5)  ; -> 252		   	     
;(binomial 13 3)  ; -> 286		   	     
;(binomial 1 1)  ; -> 1
;(binomial 1 0)  ; -> 1

;;; Description for binomial-2:   	     
  		   	     
(define binomial-2  		   	     
  (lambda (n b)
    (if (= b 0)
        1
        (* (/ n b) (binomial-2 (- n 1) (- b 1))))))
  		   	     
; Test cases for binomial-2:
  		   	     
;(binomial-2 5 1)  ; -> 5  		   	     
;(binomial-2 5 2)  ; -> 10		   	     
;(binomial-2 10 5)  ; -> 252		   	     
;(binomial-2 13 3)  ; -> 286		   	     
;(binomial-2 1 1)  ; -> 1
;(binomial-2 1 0)  ; -> 1		   	     
  		   	     
;;; Description for exactly-b-smarties:     
  		   	     
(define exactly-b-smarties  		   	     
  (lambda (n b p)
    (* (binomial-2 n b) (expt p b) (expt (- 1 p) (- n b)))))
  		   	     
; Test cases for exactly-b-smarties:
  		   	     
;(exactly-b-smarties 1 1 0.5)   ; -> 0.5
;(exactly-b-smarties 2 1 0.5)   ; -> 0.5
;(exactly-b-smarties 2 2 0.5)   ; -> 0.25
;(exactly-b-smarties 2 1 0.3)   ; -> 0.42
;(exactly-b-smarties 10 2 0.3)  ; -> 0.23347444049999985  	     
  		   	     
;;;;;;;::;;;:;::;;;;::::;;:;::;;;;:::;;;::::;:;;:;::;;;:;::;:::;::;;;:;::;:;:;::;:;;
;;; Problem 2: More Probability Theory
  		   	     
;;; Description for atleast-b-smarties:
;;; Recursive  		   	     
  		   	     
(define atleast-b-smarties  		   	     
  (lambda (n b p)
    (if (> b n)
        0
        (+ (exactly-b-smarties n b p) (atleast-b-smarties n (+ b 1) p)))))		   	     
  		   	     
; Test cases for atleast-b-smarties:
  		   	     
;(atleast-b-smarties 9 5 0.5)        ; -> 0.5
;(atleast-b-smarties 19 10 0.5)      ; -> 0.5
;(atleast-b-smarties 10 5 0.6)       ; -> 0.8337613824000002
;(atleast-b-smarties 15 5 0.3)       ; -> 0.48450894077315665
  		   	     
;;; Description for atleast-b-smarties-2:
;;; Iterative

(define atleast-b-smarties-2  		   	     
  (lambda (n b p)  		   	     
    (atleast-b-smarties-2-helper n b p 0)))

(define atleast-b-smarties-2-helper
  (lambda (n b p total)
    (if (>= b n)
        total
        (atleast-b-smarties-2-helper n (+ b 1) p (+ total (exactly-b-smarties n b p))))))
  		   	     
; Test cases for atleast-b-smarties-2:
  		   	     
;(atleast-b-smarties-2 9 5 0.5)        ; -> 0.5
;(atleast-b-smarties-2 19 10 0.5)      ; -> 0.5
;(atleast-b-smarties-2 10 5 0.6)       ; -> 0.8337613824000002
;(atleast-b-smarties-2 15 5 0.3)       ; -> 0.48450894077315665	   	     
  		   	     
;;;;;;;::;;;:;::;;;;::::;;:;::;;;;:::;;;::::;:;;:;::;;;:;::;:::;::;;;:;::;:;:;::;:;:
;;; Problem 3: Choosing a Bag  		   	     
  		   	     
;;; Description for good-bag:  		   	     
  		   	     
(define good-bag  		   	     
  (lambda (n p)
    (if (>= (atleast-b-smarties n 8 p) 0.5)
        #t
        #f)))
  		   	     
; Test cases for good-bag:  		   	     
  		   	     
;(good-bag 7 1)                  ; -> #f  		   	     
;(good-bag 8 1)                  ; -> #t  		   	     
;(good-bag 8 0.5)                ; -> #f		   	     
;(good-bag 8 0.99)               ; -> #t		   	     
;(good-bag 16 0.5)               ; -> #t		   	     
;(good-bag 16 0.7)               ; -> #t		   	     
;(good-bag 16 0.4)               ; -> #f
;(good-bag 12 0.63)              ; -> #t
;(good-bag 12 (- 0.63 0.01))     ; -> #f

;;;;;;;::;;;:;::;;;;::::;;:;::;;;;:::;;;::::;:;;:;::;;;:;::;:::;::;;;:;::;:;:;::;::;
;;; Problem 4: Choosing a Value for p
  		   	     
;;; Description of minimum-p
  		   	     
(define minimum-p  		   	     
  (lambda (n inc)  		   	     
    (minimum-p-helper n inc 0))) 		   	     

(define minimum-p-helper
  (lambda (n inc total)
    (if (good-bag n total)
        total
        (minimum-p-helper n inc (+ total inc))))) 
    
; Test cases for minimum-p:

;(minimum-p 12 0.1)    ; -> 0.7 		   	     
;(minimum-p 12 0.01)    ; -> 0.6300000000000003
;(minimum-p 12 0.001)    ; ->  	0.6220000000000004	   	     
;(minimum-p 12 0.0001)    ; ->  0.6214999999999479		   	     
;(minimum-p 12 0.00001)    ; -> 0.6214799999998064

;(minimum-p 8 0.00001)    ; -> 0.9170099999984614
;(minimum-p 10 0.00001)    ; -> 0.7414299999992605
;(minimum-p 14 0.0001)    ; -> 0.5348999999999574
  		   	     
;;;;;;;::;;;:;::;;;;::::;;:;::;;;;:::;;;::::;:;;:;::;;;:;::;:::;::;;;:;::;:;:;::;:::
;;; Problem 5: Choosing p More Efficiently
  		   	     
;;; Description of minimum-p-new
  		   	     
(define minimum-p-new  		   	     
  (lambda (n inc)  		   	     
    (minimum-p-new-helper n inc 0 0)))

(define minimum-p-new-helper
  (lambda (n inc total numCalls)
    (if (good-bag n total)
        (begin (display "value of p: ")
               (display total)
               (display " , number of calls: ")
               (display numCalls)
               (newline))
        (minimum-p-new-helper n inc (+ total inc) (+ numCalls 1)))))
  		   	     
; Test cases for minimum-p-new:
  		   	     
;(minimum-p-new 15 0.1)    ; -> value of p: 0.5 , number of calls: 5	     
;(minimum-p-new 15 0.01)    ; -> value of p: 0.5000000000000002 , number of calls: 50
;(minimum-p-new 15 0.001)    ; -> value of p: 0.5000000000000003 , number of calls: 500
;(minimum-p-new 15 0.0001)    ; -> value of p: 0.5000999999999612 , number of calls: 5001
;(minimum-p-new 15 0.00001)    ; -> value of p: 0.5000000000003593 , number of calls: 50000	   	     
  		   	     
;;; Description of minimum-p-binary:
  		   	     
(define minimum-p-binary  		   	     
  (lambda (n inc)  		   	     
    (minimum-p-binary-helper n inc 0 1 0)))		   	     
  		   	     	   	     
(define minimum-p-binary-helper
  (lambda (n inc a b count)  		   	     
    (if (< (- b a) inc)
        (begin (display "value of p: ")
               (display b)
               (display " , number of calls: ")
               (display count)
               (newline))
        (if (good-bag n (/ (+ a b) 2))
            (minimum-p-binary-helper n inc a (/ (+ a b) 2.0) (+ count 1))
            (minimum-p-binary-helper n inc (/ (+ a b) 2.0) b (+ count 1))))))
  		   	     
; Test cases for minimum-p-binary:
  		   	     
;(minimum-p-binary 12 0.1)    ; -> value of p: 0.625 , number of calls: 4
;(minimum-p-binary 12 0.01)    ; -> value of p: 0.625 , number of calls: 7
;(minimum-p-binary 12 0.001)    ; -> value of p: 0.6220703125 , number of calls: 10
;(minimum-p-binary 12 0.0001)    ; -> value of p: 0.62152099609375 , number of calls: 14
;(minimum-p-binary 12 0.00001)    ; -> value of p: 0.6214752197265625 , number of calls: 17
  		   	     
;;;;;;;::;;;:;::;;;;::::;;:;::;;;;:::;;;::::;:;;:;::;;;:;::;:::;::;;;:;::;:;:;:::;;;
;;; Problem 6: Monte-Carlo Simulations
  		   	     
;;; Description of coin-toss:
  		   	     
(define coin-toss  		   	     
  (lambda (p)  		   	     
    (>= p (random))))  	     
  		   	     
; Test cases for coin-toss:  		   	     
;(coin-toss 1)     ; -> #t
;(coin-toss 0)     ; -> #f   
  		   	     
;;; Description of random-bag:  		   	     
  		   	     
(define random-bag  		   	     
  (lambda (n p)  		   	     
    (if (= n 0)
        0
        (if (coin-toss p)
            (+ 1 (random-bag (- n 1) p))
            (+ 0 (random-bag (- n 1) p))))))
  		   	     
; Test cases for random-bag:  		   	     
;(random-bag 10 1.0)           ; -> 10		   	     
;(random-bag 20 1.0)           ; -> 20		   	     
;(random-bag 100 0.5)          ; -> 49, 56, 50, 54, etc.		   	     
;(random-bag 100 0.0)          ; -> 0	   	     
;(random-bag 10 0.0)           ; -> 0		   	     
;(random-bag 100 0.0001)       ; -> almost always 0
  		   	     
;;; Description of get-m-bags:  		   	     
  		   	     
(define get-m-bags  		   	     
  (lambda (m n p)  		   	     
    (if (= m 0)
        #f
        (if (>= (random-bag n p) 8)
            #t
            (get-m-bags (- m 1) n p)))))
            
  		   	     
; Test cases for get-m-bags:  		   	     
;(get-m-bags 1 10 1)             ; -> #t	   	     
;(get-m-bags 2 20 1)             ; -> #t		   	     
;(get-m-bags 0 10 1)             ; -> #f		   	     
;(get-m-bags 100 10 0.0001)      ; -> #f
;(get-m-bags 100 4 0.25)         ; -> #f
  		   	     
;;; Description of estimate-good-probability:
  		   	     
(define estimate-good-probability
  (lambda (m n p t)  		   	     
    (e-g-p-helper m n p t 0 0)))
        
  		   	     
(define (e-g-p-helper m n p t count numG)
  (if (= count t)
      (/ numG t)
      (if (get-m-bags m n p)
          (e-g-p-helper m n p t (+ count 1) (+ numG 1))
          (e-g-p-helper m n p t (+ count 1) numG))))
  		   	     
; Test cases for estimate-good-probability:
  		   	     
;(estimate-good-probability 24 12 0.5 1000)    ; -> 997/1000, 993/1000, 996/1000
;(estimate-good-probability 24 16 0.5 1000)    ; -> 1, 1, 1
;(estimate-good-probability 24 16 0.3 1000)    ; -> 884/1000, 857/1000, 843/1000
;(estimate-good-probability 24 16 0.2 1000)    ; -> 153/1000, 170/1000, 151/1000

;;;;;;;::;;;:;::;;;;::::;;:;::;;;;:::;;;::::;:;;:;::;;;:;::;:::;::;;;:;::;:;:;:::;;:
;;; Problem 7: Monte-Carlo Again
  		   	     
;;; Description of estimate-good-probability-2:

; - %20 blue, + %50 orange
  		   	     
(define estimate-good-probability-2
  (lambda (m n p q t)  		   	     
    (egp2Helper m n p q t 0 0)))		   	     
  		   	     
(define (egp2Helper m n p q t count numG)
  (if (= count t)
      (/ numG t)
      (if (get-m-bags-new m n p q)
          (egp2Helper m n p q t (+ count 1) (+ numG 1))
          (egp2Helper m n p q t (+ count 1) numG))))
  		   	     
(define good-bag-new ; it directly returns whether it is a good bag or not  		   	     
  (lambda (n p q o b rand count) ; b = number of blue smarties
    (if (= count n)
        (if (and (>= o (/ n 2)) (<= b (/ n 5)))
            #t
            #f)
        (cond
          ((>= p rand) (good-bag-new n p q (+ o 1) b (random) (+ count 1)))
          ((>= (+ p q) rand) (good-bag-new n p q o (+ b 1) (random) (+ count 1)))
          (else (good-bag-new n p q o b (random) (+ count 1)))))))

(define get-m-bags-new  		   	     
  (lambda (m n p q)
    (if (= m 0)
        #f
        (if (good-bag-new n p q 0 0 (random) 0)
            #t
            (get-m-bags-new (- m 1) n p q)))))

;(estimate-good-probability-2 24 12 0.5 0.5 1000)    ; -> 387/1000, 401/1000, 380/1000
;(estimate-good-probability-2 24 16 0.5 0.3 1000)    ; -> 998/1000, 999/1000, 1
;(estimate-good-probability-2 24 16 0.3 0.1 1000)    ; -> 854/1000, 857/1000, 841/1000
;(estimate-good-probability-2 1 16 0.3 0.4 1000)    ; -> 19/1000, 8/1000, 21/1000
  		   	     
;;;;;;;::;;;:;::;;;;::::;;:;::;;;;:::;;;::::;:;;:;::;;;:;::;:::;::;;;:;::;::;;::;;;;
; END OF PROJECT  		   	     
  		   	     
