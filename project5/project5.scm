;;;;;;;::;;;:;::;;;;::::;;:;::;;;;:::;;;::::;:;;:;::;;;:;::;:::;::;:::;::;::;;::;;;:
;;;   The Evaluator  		   	     
;;;;;;;::;;;:;::;;;;::::;;:;::;;;;:::;;;::::;:;;:;::;;;:;::;:::;::;:::;::;::;;::;;:;
;;;  		   	     
;;; baraci17@ku.edu.tr     Wed Dec  6 09:37:24 2017
;;;  		   	     
;;; Before you start:  		   	     
;;; * Please read the project handout available on the course
;;;   web site first to get a basic idea about the project and the
;;;   logic behind it, then to find out the details about what
;;;   your tasks are for the rest of the project.
;;;  		   	     
;;; * Use R5RS language  		   	     
;;;  		   	     
;;; * To allow redefinition, in the "Choose Language" panel, click on "Show Details"
;;;   and uncheck "Disallow redefinition of initial bindings".
;;;  		   	     
;;; * Also check the "case sensitive" choice
;;;  		   	     
;;; * The following code should be studied and loaded for this
;;;   project.  Please do not modify these files, put all your work in
;;;   this file.  		   	     
;;;  		   	     
;;; - eval.scm: code that defines the syntax, environments and control
;;;             of the basic evaluator
;;;  		   	     
;;; * Please read "Project Submission Instructions" carefully
;;;   and make sure you understand everything before you start
;;;   working on your project in order to avoid problems.
;;;  		   	     
;;; * Plan your work with pencil and paper before starting to code.
;;;  		   	     
;;; While you are working:  		   	     
;;; * If you modify a code that is already defined (e.g. m-eval), please copy the whole procedure
;;; into the corresponding part (which is defined with your-answer-here).
;;; * Type all your work and notes in the appropriate sections of this file.
;;; * Please do not delete any of the existing lines.
;;; * Remember to frequently save your file.
;;; * Use semicolon (;) to comment out text, tests and unused code.
;;; * Remember to document your code
;;; * Remember our collaboration policy: you can discuss with your friends but:
;;;  		   	     
;;;   *** NOTHING WRITTEN GETS EXCHANGED ***
;;;  		   	     
;;;  		   	     
;;; When you are done:  		   	     
;;; * Perform a final save and copy the file to the following location
;;;   F:\COURSES\UGRADS\COMP\COMP200\HOMEWORK\username\project5\project5.scm
;;;   where the username is your login name.
;;; * Please do not make any modifications after midnight on the due date.
;;; * Please send an email comp200@ku.edu.tr if you have any questions.
;;; * Make sure your file loads without errors:
;;; * You can version your code with underscore (e.g. project5_3.scm)
;;;  		   	     
;;; *** IF (load "project5.scm") GIVES ERRORS YOUR PROJECT WILL NOT BE GRADED *
;;;  		   	     
;;;  		   	     
  		   	     
;;; The following is defined so that this file loads without errors:
(define your-answer-here #f)  		   	     
  		   	     
;;;;;;;::;;;:;::;;;;::::;;:;::;;;;:::;;;::::;:;;:;::;;;:;::;:::;::;:::;::;::;;::;;::
;;; M-eval: As you work on the following exercises you will need to
;;; modify m-eval multiple times.  Below is a copy from eval.scm for
;;; you to modify as needed.  First we load the original definition:
  		   	     
(load "eval.scm")  		   	     
  		   	     
;;;;;;;::;;;:;::;;;;::::;;:;::;;;;:::;;;::::;:;;:;::;;;:;::;:::;::;:::;::;::;;::;:;;
;;; Then we override it with our own definition:
  		   	     
(define (m-eval exp env)  		   	     
  (cond ((self-evaluating? exp) exp)
        ((variable? exp) (lookup-variable-value exp env))
        ((quoted? exp) (text-of-quotation exp))
        ((assignment? exp) (eval-assignment exp env))
        ((definition? exp) (eval-definition exp env))
        ((if? exp) (eval-if exp env))
        ((lambda? exp)  		   	     
         (make-procedure (lambda-parameters exp) (lambda-body exp) env))
        ((begin? exp) (eval-sequence (begin-actions exp) env))
        ((cond? exp) (m-eval (cond->if exp) env))
	((let? exp) (m-eval (let->application exp) env))
        ((application? exp)  		   	     
         (m-apply (m-eval (operator exp) env)
                (list-of-values (operands exp) env)))
        (else (error "Unknown expression type -- EVAL" exp))))
  		   	     
;;;;;;;::;;;:;::;;;;::::;;:;::;;;;:::;;;::::;:;;:;::;;;:;::;:::;::;:::;::;::;;::;:;:
;;; Computer Exercise 1: Adding new primitive procedures
;;;  		   	     
;;; Please find and copy the code from eval.scm where new procedures
;;; can be added below.  Then modify it to extend the evaluator as
;;; described in the exercise.  		   	     
  		   	     
; New code:  		   	     

(define (primitive-procedures)  		   	     
  (list (list 'car car)  		   	     
        (list 'cdr cdr)  		   	     
        (list 'cons cons)  		   	     
        (list 'set-car! set-car!)
        (list 'set-cdr! set-cdr!)
        (list 'null? null?)  		   	     
        (list '+ +)  		   	     
        (list '- -)
        (list '* *)
        (list '/ /)
        (list '< <)  		   	     
        (list '> >)  		   	     
        (list '= =)  		   	     
      	(list 'display display)
        (list 'not not)
        (list 'cadr cadr)
        (list 'cddr cddr)
        (list 'newline newline)
        (list 'length length)
        ))

; Test cases:

;(refresh-global-environment)

;(m-eval '(* 3 4) the-global-environment) ; 12
;(m-eval '(cadr '(a b c d)) the-global-environment) ; b
;(m-eval '(length '(1 2 3 4)) the-global-environment) ; 4

;;;;;;;::;;;:;::;;;;::::;;:;::;;;;:::;;;::::;:;;:;::;;;:;::;:::;::;:::;::;::;;::;::;
  		   	     
  		   	     
  		   	     
;;;;;;;::;;;:;::;;;;::::;;:;::;;;;:::;;;::::;:;;:;::;;;:;::;:::;::;:::;::;::;;::;:::
;;; Computer Exercise 2: The reset! special form
;;;  		   	     
;;; Please find and copy the relevant code from eval.scm below.  Then
;;; make the necessary modifications and additions to handle the
;;; reset! form.  Do not forget to modify the definition of m-eval
;;; above to handle the new form.
;;;;;;;::;;;:;::;;;;::::;;:;::;;;;:::;;;::::;:;;:;::;;;:;::;:::;::;:::;::;::;;:::;;;
  		   	     
  		   	     
;;;;;;;::;;;:;::;;;;::::;;:;::;;;;:::;;;::::;:;;:;::;;;:;::;:::;::;:::;::;::;;:::;;:
; New code:

(define (m-eval exp env)  		   	     
  (cond ((self-evaluating? exp) exp)
        ((variable? exp) (lookup-variable-value exp env))
        ((quoted? exp) (text-of-quotation exp))
        ((assignment? exp) (eval-assignment exp env))
        ((definition? exp) (eval-definition exp env))
        ((if? exp) (eval-if exp env))
        ((lambda? exp)  		   	     
         (make-procedure (lambda-parameters exp) (lambda-body exp) env))
        ((begin? exp) (eval-sequence (begin-actions exp) env))
        ((cond? exp) (m-eval (cond->if exp) env))
	((let? exp) (m-eval (let->application exp) env))
        ((reset!? exp) (eval-reset! exp env))
        ((application? exp)  		   	     
         (m-apply (m-eval (operator exp) env)
                (list-of-values (operands exp) env)))
        (else (error "Unknown expression type -- EVAL" exp))))

(define (reset!? exp) (tagged-list? exp 'reset!))
(define (reset-exp exp) (cadr exp))

(define (set-binding-value! binding val)
  (set-cdr! binding (cons val (cdr binding))))

(define (reset-binding-value! binding)
  (if (not (null? (cddr binding)))
      (set-cdr! binding (cddr binding))))

(define (eval-reset! exp env)
  (if (eq? env the-empty-environment)
      (error "No value to reset!")
	(if (find-in-frame (reset-exp exp) (first-frame env))
	    ((lambda (binding val) (reset-binding-value! binding)) (find-in-frame (reset-exp exp) (first-frame env)) '())
	    (set-variable-value! (reset-exp exp) '() (enclosing-environment env)))))

;;;;;;;::;;;:;::;;;;::::;;:;::;;;;:::;;;::::;:;;:;::;;;:;::;:::;::;:::;::;:::;::;;;;
  		   	     
  		   	     
;;;;;;;::;;;:;::;;;;::::;;:;::;;;;:::;;;::::;:;;:;::;;;:;::;:::;::;:::;::;:::;::;;;:
; Test cases:  		   	     

(refresh-global-environment)

(m-eval '(define a 1) the-global-environment)
(m-eval '(set! a 2) the-global-environment)
(m-eval '(set! a 3) the-global-environment)
(m-eval '(set! a 4) the-global-environment)
(m-eval '(display a) the-global-environment) ; 4
(m-eval '(reset! a) the-global-environment)
(m-eval '(display a) the-global-environment) ; 3
(m-eval '(reset! a) the-global-environment)
(m-eval '(reset! a) the-global-environment)
(m-eval '(display a) the-global-environment) ; 1

;;;;;;;::;;;:;::;;;;::::;;:;::;;;;:::;;;::::;:;;:;::;;;:;::;:::;::;:::;::;:::;::;;:;
  		   	     
  		   	     
  		   	     
;;;;;;;::;;;:;::;;;;::::;;:;::;;;;:::;;;::::;:;;:;::;;;:;::;:::;::;:::;::;:::;::;;::
;;; Computer Exercise 3: The loop special form
;;;  		   	     
;;; To create the loop special form, you need to:
;;; 1. Create a data abstraction for handling loops
;;; 2. Write the procedures that handle the evaluation of loops
;;; 3. Modify m-eval to handle this new special form
;;; Following the instructions given in the exercise.
;;;;;;;::;;;:;::;;;;::::;;:;::;;;;:::;;;::::;:;;:;::;;;:;::;:::;::;:::;::;:::;::;:;;
  	
  		   	     
;;;;;;;::;;;:;::;;;;::::;;:;::;;;;:::;;;::::;:;;:;::;;;:;::;:::;::;:::;::;:::;::;:;:
; New code:

(define (m-eval exp env)  		   	     
  (cond ((self-evaluating? exp) exp)
        ((variable? exp) (lookup-variable-value exp env))
        ((quoted? exp) (text-of-quotation exp))
        ((assignment? exp) (eval-assignment exp env))
        ((definition? exp) (eval-definition exp env))
        ((if? exp) (eval-if exp env))
        ((lambda? exp)  		   	     
         (make-procedure (lambda-parameters exp) (lambda-body exp) env))
        ((begin? exp) (eval-sequence (begin-actions exp) env))
        ((cond? exp) (m-eval (cond->if exp) env))
	((let? exp) (m-eval (let->application exp) env))
        ((reset!? exp) (eval-reset! exp env))
        ((loop? exp) (eval-loop exp env))
        ((application? exp)  		   	     
         (m-apply (m-eval (operator exp) env)
                (list-of-values (operands exp) env)))
        (else (error "Unknown expression type -- EVAL" exp))))

(define (loop? exp) (tagged-list? exp 'loop))
(define (loop-exps exp) (cdddr exp))
(define (loop-until exp) (cadr exp))
(define (loop-return exp) (caddr exp))

(define (eval-loop exp env)
  (if (m-eval (loop-until exp) env)
      (m-eval (loop-return exp) env)
      (begin (eval-sequence (loop-exps exp) env) (eval-loop exp env))))

;;;;;;;::;;;:;::;;;;::::;;:;::;;;;:::;;;::::;:;;:;::;;;:;::;:::;::;:::;::;:::;::;::;
  		   	     
  		   	     
;;;;;;;::;;;:;::;;;;::::;;:;::;;;;:::;;;::::;:;;:;::;;;:;::;:::;::;:::;::;:::;::;:::
; Test cases:

;(refresh-global-environment)

;(m-eval '(define a 2) the-global-environment)
;(m-eval '(define b 4) the-global-environment)
(define loop-ex '(loop (> a 10) (+ a b)
                       (set! a (+ a 1))
                       (set! b (+ b 2))))

;(m-eval loop-ex the-global-environment) ; 33

;;;;;;;::;;;:;::;;;;::::;;:;::;;;;:::;;;::::;:;;:;::;;;:;::;:::;::;:::;::;:::;:::;;;
  		   	     
  		   	     
  		   	     
;;;;;;;::;;;:;::;;;;::::;;:;::;;;;:::;;;::::;:;;:;::;;;:;::;:::;::;:::;::;:::;:::;;:
;;; Computer Exercise 4: Transforming "and" and "or"
;;;  		   	     
;;; Please fill in the following code to transform "and" and "or"
;;; expressions into "if" expressions:
;;;;;;;::;;;:;::;;;;::::;;:;::;;;;:::;;;::::;:;;:;::;;;:;::;:::;::;:::;:::;;;;::;;;;


;;;;;;;::;;;:;::;;;;::::;;:;::;;;;:::;;;::::;:;;:;::;;;:;::;:::;::;:::;::;:::;::;:;:
; New code:
  		   	     
(define (m-eval exp env)  		   	     
  (cond ((self-evaluating? exp) exp)
        ((variable? exp) (lookup-variable-value exp env))
        ((quoted? exp) (text-of-quotation exp))
        ((assignment? exp) (eval-assignment exp env))
        ((definition? exp) (eval-definition exp env))
        ((if? exp) (eval-if exp env))
        ((lambda? exp)  		   	     
         (make-procedure (lambda-parameters exp) (lambda-body exp) env))
        ((begin? exp) (eval-sequence (begin-actions exp) env))
        ((cond? exp) (m-eval (cond->if exp) env))
	((let? exp) (m-eval (let->application exp) env))
        ((reset!? exp) (eval-reset! exp env))
        ((loop? exp) (eval-loop exp env))
        ((and? exp) (m-eval (and->if exp) env))
        ((or? exp) (m-eval (or->if exp) env))
        ((application? exp)  		   	     
         (m-apply (m-eval (operator exp) env)
                (list-of-values (operands exp) env)))
        (else (error "Unknown expression type -- EVAL" exp))))

(define (and? expr) (tagged-list? expr 'and))
(define (or? expr) (tagged-list? expr 'or))

(define (predicate exp) (cadr exp))
(define (or-body exp) (cdr (or-exprs exp)))
(define (and-body exp) (cdr (and-exprs exp)))
(define (and-main exp) (cadr (and-exprs exp)))

(define (or->if exp)
  (if (null? (cdr (or-body exp)))
      (predicate exp)
      (make-if (predicate exp)
               (predicate exp)
               (make-or (or-body exp)))))

(define (and->if exp)
  (if (not (null? (cdr (and-body exp))))
      (make-if (predicate exp)
               (and-main exp)
               (make-begin (and-body exp)))))

;;;;;;;::;;;:;::;;;;::::;;:;::;;;;:::;;;::::;:;;:;::;;;:;::;:::;::;:::;:::;;;;::;:;;


;;;;;;;::;;;:;::;;;;::::;;:;::;;;;:::;;;::::;:;;:;::;;;:;::;:::;::;:::;:::;;;;::;:::
;;; Modify m-eval to handle "and" and "or" as special forms and give
;;; some test examples:		   	     

;(refresh-global-environment)

(define or-ex '(or (begin (display "once ") #f)
                    (begin (display "and ") #f)
                    (begin (display "only ") 'done)
                    (begin (display "adbmal") #t)))

(define and-ex '(and (begin (display "a ") (> 6 5))
                     (begin (display "b ") (< 6 5))
                     (begin (display "c ") (+ 6 5))))

;(m-eval or-ex the-global-environment)
;(m-eval and-ex the-global-environment)

;;;;;;;::;;;:;::;;;;::::;;:;::;;;;:::;;;::::;:;;:;::;;;:;::;:::;::;:::;:::;;;;:::;;;
  		   	     
  		   	     
  		   	     
;;;;;;;::;;;:;::;;;;::::;;:;::;;;;:::;;;::::;:;;:;::;;;:;::;:::;::;:::;:::;;;;:::;;:
;;; Computer Exercise 5: Transforming "loop2"
;;;  		   	     
;;; Define the converter that will transform a loop2 expression into an
;;; "if" expression:  		   	     
;;;;;;;::;;;:;::;;;;::::;;:;::;;;;:::;;;::::;:;;:;::;;;:;::;:::;::;:::;:::;;:;::;;;;
  		   	     
  		   	     		   	     
;;;;;;;::;;;:;::;;;;::::;;:;::;;;;:::;;;::::;:;;:;::;;;:;::;:::;::;:::;:::;;:;::;;;:
; New code:

(define (m-eval exp env)  		   	     
  (cond ((self-evaluating? exp) exp)
        ((variable? exp) (lookup-variable-value exp env))
        ((quoted? exp) (text-of-quotation exp))
        ((assignment? exp) (eval-assignment exp env))
        ((definition? exp) (eval-definition exp env))
        ((if? exp) (eval-if exp env))
        ((lambda? exp)  		   	     
         (make-procedure (lambda-parameters exp) (lambda-body exp) env))
        ((begin? exp) (eval-sequence (begin-actions exp) env))
        ((cond? exp) (m-eval (cond->if exp) env))
	((let? exp) (m-eval (let->application exp) env))
        ((reset!? exp) (eval-reset! exp env))
        ((loop? exp) (m-eval (loop->if exp) env))
        ((and? exp) (m-eval (and->if exp) env))
        ((or? exp) (m-eval (or->if exp) env))
        ((application? exp)  		   	     
         (m-apply (m-eval (operator exp) env)
                (list-of-values (operands exp) env)))
        (else (error "Unknown expression type -- EVAL" exp))))

(define (loop->if exp)
  (make-if (loop-until exp)
           (loop-return exp)
           (make-begin (append (loop-exps exp) (list exp)))))

;;;;;;;::;;;:;::;;;;::::;;:;::;;;;:::;;;::::;:;;:;::;;;:;::;:::;::;:::;:::;;:;::;;:;	   	     
  		   	     
  		   	     
;;;;;;;::;;;:;::;;;;::::;;:;::;;;;:::;;;::::;:;;:;::;;;:;::;:::;::;:::;:::;;:;::;;::
;;; Modify m-eval to handle loop2 as described in the exercise and give
;;; some test examples below:  		   	     

;(refresh-global-environment)

;(m-eval '(define a 2) the-global-environment)
;(m-eval '(define b 4) the-global-environment)

(define loop-ex '(loop (> a 10) (+ a b)
                       (set! a (+ a 1))
                       (set! b (+ b 2))))

;(m-eval loop-ex the-global-environment) ; 33

;;;;;;;::;;;:;::;;;;::::;;:;::;;;;:::;;;::::;:;;:;::;;;:;::;:::;::;:::;:::;;:;::;:;;
  		   	     
  		   	     
  		   	     
;;;;;;;::;;;:;::;;;;::::;;:;::;;;;:::;;;::::;:;;:;::;;;:;::;:::;::;:::;:::;;:;::;:;:
;;; Computer Exercise 6: Adding memoization to the evaluator
;;;  		   	     
;;; Please find and copy all relevant parts of eval.scm below.  Then
;;; make the necessary modifications and additions to add memoization
;;; to the evaluator as described in the exercise.
;;;;;;;::;;;:;::;;;;::::;;:;::;;;;:::;;;::::;:;;:;::;;;:;::;:::;::;:::;:::;;:;::;::;	   	     
  		   	     
  		   	     
;;;;;;;::;;;:;::;;;;::::;;:;::;;;;:::;;;::::;:;;:;::;;;:;::;:::;::;:::;:::;;:;::;:::
; New code:  		   	     
;your-answer-here  		   	     
;;;;;;;::;;;:;::;;;;::::;;:;::;;;;:::;;;::::;:;;:;::;;;:;::;:::;::;:::;:::;;:;:::;;;		   	     
  		   	     
  		   	     
;;;;;;;::;;;:;::;;;;::::;;:;::;;;;:::;;;::::;:;;:;::;;;:;::;:::;::;:::;:::;;:;:::;;:
; Test cases:  		   	     
;your-answer-here  		   	     
;;;;;;;::;;;:;::;;;;::::;;:;::;;;;:::;;;::::;:;;:;::;;;:;::;:::;:::;;;;::;;;;;::;;;;



;;;;;;;::;::::::;;;::::;;;;:::;:::;;::;;;:;::;;::;::;:;;;:::;;:;::;::;;::;::;
;# DO NOT FORGET TO SUBMIT YOUR WORK BEFORE THE DEADLINE!         #
;;;;;;;::;::::::;;;::::;;;;:::;:::;;::;;;:;::;;::;::;:;;;:::;;:;::;::;;::;:::
;# GOOD LUCK!                                                     #
;;;;;;;::;::::::;;;::::;;;;:::;:::;;::;;;:;::;;::;::;:;;;:::;;:;::;::;;:::;;;
