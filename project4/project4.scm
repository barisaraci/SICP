;;;;;;;::;;;:;::;;;;::::;;:;::;;;;:::;;;::::;:;;:;::;;;:;::;:::;::;;;:;::;;;;;::;;:;;::;:::
;;;   The Object-Oriented Adventure Game
;;;;;;;::;;;:;::;;;;::::;;:;::;;;;:::;;;::::;:;;:;::;;;:;::;:::;::;;;:;::;;;;;::;;:;;:::;;;
;;;  		   	     
;;; baraci17@ku.edu.tr     Mon Nov 20 18:12:04 2017
;;;  		   	     
;;; Before you start:  		   	     
;;; * Please read the project handout available on the course
;;;   web site first to get a basic idea about the project and the
;;;   logic behind it, then to find out the details about what
;;;   your tasks are for the rest of the project.
;;;  		   	     
;;; * The following code should be studied and loaded for this
;;;   project.  Please do not modify these files, put all your work in
;;;   this file.  		   	     
;;;  		   	     
;;; - objsys.scm: support for an elementary object system
;;; - objtypes.scm: a few nice object classes
;;; - setup.scm: a bizarre world constructed using these classes
;;;  		   	     
;;; * Plan your work with pencil and paper before starting to code.
;;;  		   	     
;;; While you are working:  		   	     
;;; * Type all your work and notes in the appropriate sections of this file.
;;; * Please do not delete any of the existing lines.
;;; * Use the procedure names given in the instructions.
;;; * Remember to frequently save your file.
;;; * Use semicolon (;) to comment out text, tests and unused code.
;;; * Remember to document your code (in this file)
;;; * Remember our collaboration policy: you can discuss with your friends but:
;;;  		   	     
;;;   *** NOTHING WRITTEN GETS EXCHANGED ***
;;;  		   	     
;;;  		   	     
;;; When you are done:  		   	     
;;; * Perform a final save and submit your work following the instructions.
;;; * Please do not make any modifications after midnight on the due date.
;;; * Please send an email comp200-common@ku.edu.tr if you have any questions.
;;; * Make sure your file loads without errors:
;;; ****************************************************************************
;;; ***  Your code should run without any syntactic errors. Projects  causing error will NOT be graded. ***
;;; ****************************************************************************
;;;  		   	     
;; Do NOT modify or delete the lines below.
(#%require (only racket/base random))
(load "objsys.scm")  		   	     
(load "objtypes.scm")  		   	     
(load "setup.scm")  		   	     
(define nil '())  		   	     
(define your-answer-here #f)  		   	     
;;;;;;;;;  		   	     
  		   	     
;;;;;;;::;;;:;::;;;;::::;;:;::;;;;:::;;;::::;:;;:;::;;;:;::;:::;::;;;:;::;;;;;::;;:;;:::;;:
;;; PART II. Programming Assignment
;;;  		   	     
;;; The answers to the computer exercises in Section 5 go in the
;;; appropriate sections below.
;;;  		   	     
  		   	     
  		   	     
  		   	     
;;;;;;;::;;;:;::;;;;::::;;:;::;;;;:::;;;::::;:;;:;::;;;:;::;:::;::;;;:;::;;;;;::;;::;::;;;;
;;;;;;;;;;;;; Computer Exercise 0: Setting up ;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;  		   	     
;;;;;;;;;;;;; CODES: ;;;;;;;;;;;;;
;;

;(ask screen 'deity-mode #f)
;(setup 'baris)
;(ask (ask me 'location) 'name)
;(ask me 'name)
;(ask me 'say (list 'hello 'world))
;(ask me 'go (ask (car (ask (ask me 'location) 'exits)) 'name))
;((lambda ()                                                                    ; I have created a procedure to check whether there is any movable item in the location or not.
;   (let ((item (ask me 'stuff-around)))
;     (if (and (not (null? item)) (is-a (car item) 'mobile-thing?))
;         (begin
;           (ask me 'take (car item))
;           (ask me 'toss (car item)))))))
;(ask me 'die)

;;;;;;;::;;;:;::;;;;::::;;:;::;;;;:::;;;::::;:;;:;::;;;:;::;:::;::;;;:;::;;;;;::;;::;::;;;:
;;;;;;;;;;;;; TRANSCRIPT: ;;;;;;;;;;;;;
;;  		   	   		   	     

;;;; value: ready
;;;; value: gym
;;;; value: baris

;;At gym baris says -- hello world 
;;baris moves from gym to library 
;;--- the-clock Tick 0 --- 
;;You are in library 
;;You are not holding anything. 
;;You see stuff in the room: engineering-book 
;;There are no other people around you. 
;;The exits are in directions: west east

;;;; value: #t

;;At library baris says -- I take engineering-book from library 
;;At library baris says -- i do not have a #<procedure:...ct4/objtypes.scm:105:4> 
;;At library baris says -- SHREEEEK!  I, uh, suddenly feel very faint... 
;;At library baris says -- I lose engineering-book 
;;At library baris says -- Yaaaah! I am upset! 
;;An earth-shattering, soul-piercing scream is heard... 
;;baris moves from library to heaven

;;;; value: game-over-for-you-dude

;;  		   	     
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;; END ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;::;;;:;::;;;;::::;;:;::;;;;:::;;;::::;:;;:;::;;;:;::;:::;::;;;:;::;;;;;::;;::;::;;:;
  		   	     
  		   	     
  		   	     
  		   	     
  		   	     
  		   	     
  		   	     
  		   	     
;;;;;;;::;;;:;::;;;;::::;;:;::;;;;:::;;;::::;:;;:;::;;;:;::;:::;::;;;:;::;;;;;::;;::;::;;::
;;;;;; Computer Exercise 1: Understanding installation;;;;;
;;;;;;;::;;;:;::;;;;::::;;:;::;;;;:::;;;::::;:;;:;::;;;:;::;:::;::;;;:;::;;;;;::;;::;::;:;;
;;  		   	     
;;;;;;;;;;;;; ANSWER: ;;;;;;;;;;;;;
;;  		   	     
  		   	     
; It is about inheritance. If we use "ask", both the superclass and the internal class will call the function.
; But, if we use "delegate", the class inside the second parameter will apply the method comes from the class inside the third parameter.
  		   	     
;;  		   	     
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;; END ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;::;;;:;::;;;;::::;;:;::;;;;:::;;;::::;:;;:;::;;;:;::;:::;::;;;:;::;;;;;::;;::;::;:;:
  		   	     
  		   	     
  		   	     
  		   	     
  		   	     
  		   	     
  		   	     
  		   	     
  		   	     
  		   	     
  		   	     
  		   	     
;;;;;;;::;;;:;::;;;;::::;;:;::;;;;:::;;;::::;:;;:;::;;;:;::;:::;::;;;:;::;;;;;::;;::;::;::;
;;;;;;;;;;;; Computer Exercise 2: Who just died? ;;;;;;;;;;
;;;;;;;::;;;:;::;;;;::::;;:;::;;;;:::;;;::::;:;;:;::;;;:;::;:::;::;;;:;::;;;;;::;;::;::;:::
;;  		   	     
;;;;;;;;;;;;; CODE: ;;;;;;;;;;;;;
;;  		   	     

(define whoDied (lambda ()
                  (let ((personDead (ask heaven 'things)))
                    (if (null? personDead)
                        (begin
                          (run-clock 1)
                          (whoDied))
                        (ask (car personDead) 'name)))))
  		   	     
;;;;;;;::;;;:;::;;;;::::;;:;::;;;;:::;;;::::;:;;:;::;;;:;::;:::;::;;;:;::;;;;;::;;::;:::;;;
;;;;;;;;;;;;; EXPLANATION: ;;;;;;;;;;;;;
;;  		   	     
  		   	     
; Heaven is a place including things inside a container. When "die" method of an object is called, the object goes to heaven.
; So if we check the things inside heaven every "run-clock" and if the list is not null, then we can conclude that someone is dead.
  		   	     
;;;;;;;::;;;:;::;;;;::::;;:;::;;;;:::;;;::::;:;;:;::;;;:;::;:::;::;;;:;::;;;;;::;;::;:::;;:
;;;;;;;;;;;;; TRANSCRIPT: ;;;;;;;;;;;;;
;;  		   	     

;(ask screen 'deity-mode #f)
;(setup 'baris)

;;;; value: ready

;(whoDied)

;;--- the-clock Tick 0 --- 
;;--- the-clock Tick 1 --- 
;;--- the-clock Tick 2 --- 
;;--- the-clock Tick 3 --- 
;;--- the-clock Tick 4 --- 
;;An earth-shattering, soul-piercing scream is heard... 
;;--- the-clock Tick 5 ---

;;;; value: alyssa-p-hacker	   	     
  		   	     
;;  		   	     
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;; END ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;::;;;:;::;;;;::::;;:;::;;;;:::;;;::::;:;;:;::;;;:;::;:::;::;;;:;::;;;;;::;:;;;::;;;;
  		   	     
  		   	     
  		   	     
  		   	     
  		   	     
  		   	     
  		   	     
  		   	     
  		   	     
  		   	     
;;;;;;;::;;;:;::;;;;::::;;:;::;;;;:::;;;::::;:;;:;::;;;:;::;:::;::;;;:;::;;;;;::;:;;;::;;;:
;;;;;;;; Computer exercise 3: Having a quick look ;;;;;;;;;
;;;;;;;::;;;:;::;;;;::::;;:;::;;;;:::;;;::::;:;;:;::;;;:;::;:::;::;;;:;::;;;;;::;:;;;::;;:;
;;  		   	     
;;;;;;;;;;;;; CODE: ;;;;;;;;;;;;;
;;  		   	     
  			   	     
;((GO)  		   	     
;	 (lambda (self direction)  ; Shadows person's GO
;	   (let ((success? (delegate person-part self 'GO direction)))
;	     (if success?  		   	     
;                 (begin (ask clock 'TICK)
;                        (ask self 'LOOK-AROUND)));; I changed this line to look around.   <--- here it was already added
;	     success?)))		   	     
  		   	     
;;;;;;;::;;;:;::;;;;::::;;:;::;;;;:::;;;::::;:;;:;::;;;:;::;:::;::;;;:;::;;;;;::;:;;;::;;::
;;;;;;;;;;;;; EXPLANATION: ;;;;;;;;;;;;;
;;  		   	     
  		   	     
; As I see it, the necessary code line was already added to GO part of avatar definition in objtypes.scm.
; When avatar calls GO, avatar calls LOOK-AROUND as well if the move is successful.
  		   	     
;;;;;;;::;;;:;::;;;;::::;;:;::;;;;:::;;;::::;:;;:;::;;;:;::;:::;::;;;:;::;;;;;::;:;;;::;:;;
;;;;;;;;;;;;; TRANSCRIPT: ;;;;;;;;;;;;;
;;  		   	     
  		   	     
;(ask screen 'deity-mode #f)
;(setup 'baris)

;;;; value: ready

;(ask me 'go (ask (car (ask (ask me 'location) 'exits)) 'name))

;;baris moves from amphitheater to sos-building 
;;--- the-clock Tick 0 --- 
;;You are in sos-building 
;;You are not holding anything. 
;;There is no stuff in the room. 
;;There are no other people around you. 
;;The exits are in directions: east south north

;;;; value: #t
  		   	     
;;  		   	     
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;; END ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;::;;;:;::;;;;::::;;:;::;;;;:::;;;::::;:;;:;::;;;:;::;:::;::;;;:;::;;;;;::;:;;;::;:;:
  		   	     
  		   	     
  		   	     
  		   	     
  		   	     
  		   	     
  		   	     
  		   	     
  		   	     
  		   	     
  		   	     
;;;;;;;::;;;:;::;;;;::::;;:;::;;;;:::;;;::::;:;;:;::;;;:;::;:::;::;;;:;::;;;;;::;:;;;::;::;
;;;;;; Computer exercise 4: But I'm too young to die!! ;;;;
;;;;;;;::;;;:;::;;;;::::;;:;::;;;;:::;;;::::;:;;:;::;;;:;::;:::;::;;;:;::;;;;;::;:;;;::;:::
;;  		   	     
;;;;;;;;;;;;; CODE: ;;;;;;;;;;;;;
;;  		   	     
  		   	     
(define (make-person name birthplace) ; symbol, location -> person
  (let ((mobile-thing-part (make-mobile-thing name birthplace))
        (container-part    (make-container))
        (health            3)  		   	     
        (strength          1)  		   	     
        (lives             3))  		   	     
    (lambda (message)  		   	     
      (case message  		   	     
        ((PERSON?) (lambda (self) #T))
        ((STRENGTH) (lambda (self) strength))
        ((HEALTH) (lambda (self) health))
        ((SAY)  		   	     
         (lambda (self list-of-stuff)
           (ask screen 'TELL-ROOM (ask self 'location)
                (append (list "At" (ask (ask self 'LOCATION) 'NAME)
                              (ask self 'NAME) "says --")
                        list-of-stuff))
           'SAID-AND-HEARD))
        ((HAVE-FIT)  		   	     
         (lambda (self)  		   	     
           (ask self 'SAY '("Yaaaah! I am upset!"))
           'I-feel-better-now))
  		   	     
        ((PEOPLE-AROUND)	; other people in room...
         (lambda (self)  		   	     
           (let* ((in-room (ask (ask self 'LOCATION) 'THINGS))
                  (people (myfilter (lambda (x) (is-a x 'PERSON?)) in-room)))
             (delq self people))))
  		   	     
        ((STUFF-AROUND)		; stuff (non people) in room...
         (lambda (self)  		   	     
           (let* ((in-room (ask (ask self 'LOCATION) 'THINGS))
                  (stuff (myfilter (lambda (x) (not (is-a x 'PERSON?))) in-room)))
             stuff)))  		   	     
  		   	     
        ((PEEK-AROUND)		; other people's stuff...
         (lambda (self)  		   	     
           (let ((people (ask self 'PEOPLE-AROUND)))
             (accumulate append '() (map (lambda (p) (ask p 'THINGS)) people)))))
  		   	     
        ((TAKE)  		   	     
         (lambda (self thing)  		   	     
           (cond ((ask self 'HAVE-THING? thing)  ; already have it
                  (ask self 'SAY (list "I am already carrying"
                                       (ask thing 'NAME)))
                  #f)  		   	     
                 ((or (is-a thing 'PERSON?)
                      (not (is-a thing 'MOBILE-THING?)))
                  (ask self 'SAY (list "I try but cannot take"
                                       (ask thing 'NAME)))
                  #F)  		   	     
                 (else  		   	     
                  (let ((owner (ask thing 'LOCATION)))
                    (ask self 'SAY (list "I take" (ask thing 'NAME)
                                         "from" (ask owner 'NAME)))
                    (if (is-a owner 'PERSON?)
                        (ask owner 'LOSE thing self)
                        (ask thing 'CHANGE-LOCATION self))
                    thing)))))  		   	     
  		   	     
        ((LOSE)  		   	     
         (lambda (self thing lose-to)
           (ask self 'SAY (list "I lose" (ask thing 'NAME)))
           (ask self 'HAVE-FIT)
           (ask thing 'CHANGE-LOCATION lose-to)))
  		   	     
        ((DROP)  		   	     
         (lambda (self thing)  		   	     
           (ask self 'SAY (list "I drop" (ask thing 'NAME)
                                "at" (ask (ask self 'LOCATION) 'NAME)))
           (ask thing 'CHANGE-LOCATION (ask self 'LOCATION))))
  		   	     
        ((GO-EXIT)  		   	     
         (lambda (self exit)  		   	     
           (ask exit 'USE self)))
  		   	     
        ((GO)  		   	     
         (lambda (self direction) ; person, symbol -> boolean
           (let ((exit (ask (ask self 'LOCATION) 'EXIT-TOWARDS direction)))
             (if (is-a exit 'EXIT?)
                 (ask self 'GO-EXIT exit)
                 (begin (ask screen 'TELL-ROOM (ask self 'LOCATION)
                             (list "No exit in" direction "direction"))
                        #F)))))
        ((SUFFER)  		   	     
         (lambda (self hits)  		   	     
           (ask self 'SAY (list "Ouch!" hits "hits is more than I want!"))
           (set! health (- health hits))
           (if (<= health 0) (ask self 'DIE))
           health))  		   	     
  		   	     
        ((DEATH-SCREAM)  		   	     
         (lambda (self)  		   	     
           (ask screen 'TELL-WORLD
                '("An earth-shattering, soul-piercing scream is heard..."))))
  		   	     
        ((ENTER-ROOM)  		   	     
         (lambda (self)  		   	     
           (let ((others (ask self 'PEOPLE-AROUND)))
             (if (not (null? others))
                 (ask self 'SAY (cons "Hi" (names-of others)))))
           (ask (ask self 'location) 'make-noise self)
           #T))  		   	     
  		   	     
        ;; Here is the original DIE method
        #|  		   	     
	 ((DIE)  		   	     
	  (lambda (self)  		   	     
	    (ask self 'SAY '("SHREEEEK!  I, uh, suddenly feel very faint..."))
	    (for-each (lambda (item) (ask self 'LOSE item (ask self 'LOCATION)))
	 	     (ask self 'THINGS))
	    (ask self 'DEATH-SCREAM)
	    (ask death-exit 'USE self)
	    'GAME-OVER-FOR-YOU-DUDE))
   |#  		   	     
        ;; Your version goes here:
  		   	     
        ((DIE)  		   	     
         (lambda (self)
           (for-each (lambda (item) (ask self 'LOSE item (ask self 'LOCATION)))
                     (ask self 'THINGS))
           (if (= lives 1)
               (begin
                 (ask self 'say '("SHREEEEK!  I, uh, suddenly feel very faint..."))
                 (ask self 'death-scream)
                 (ask death-exit 'use self)
                 'GAME-OVER-FOR-YOU-DUDE)
               (begin (set! lives (- lives 1))
                      (set! health 3)
                      (ask self 'change-location (ask self 'creation-site))
                      (ask self 'say (list "I have" lives "more lives"))))))
  		   	     
        (else (find-method message mobile-thing-part container-part))))))

;;;;;;;::;;;:;::;;;;::::;;:;::;;;;:::;;;::::;:;;:;::;;;:;::;:::;::;;;:;::;;;;;::;:;;;:::;;;
;;;;;;;;;;;;; EXPLANATION: ;;;;;;;;;;;;;
;;  		   	     
  		   	     
; If remaining lives is equal to 1, then the person dies. Otherwise, the person lose their possessions and a lives and go to the birthplace. 	   	     
  		   	     
;;;;;;;::;;;:;::;;;;::::;;:;::;;;;:::;;;::::;:;;:;::;;;:;::;:::;::;;;:;::;;;;;::;:;;;:::;;:
;;;;;;;;;;;;; TRANSCRIPT: ;;;;;;;;;;;;;
;;

;(ask screen 'deity-mode #f)
;(setup 'baris)

;;;; value: ready

;(ask me 'die)

;;At eng-auditorium baris says -- I have 2 more lives
;;;; value: said-and-heard

;(ask me 'die)

;;At eng-auditorium baris says -- I have 1 more lives
;;;; value: said-and-heard

;(ask me 'die)

;;At eng-auditorium baris says -- SHREEEEK!  I, uh, suddenly feel very faint... 
;;An earth-shattering, soul-piercing scream is heard... 
;;baris moves from eng-auditorium to heaven
;;;; value: game-over-for-you-dude
  		   	     
;;  		   	     
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;; END ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;::;;;:;::;;;;::::;;:;::;;;;:::;;;::::;:;;:;::;;;:;::;:::;::;;;:;::;;;;;::;:;:;::;;;;
  		   	     
  		   	     
  		   	     
  		   	     
  		   	     
  		   	     
  		   	     
  		   	     
  		   	     
  		   	     
;;;;;;;::;;;:;::;;;;::::;;:;::;;;;:::;;;::::;:;;:;::;;;:;::;:::;::;;;:;::;;;;;::;:;:;::;;;:
;;; Computer exercise 5: Perhaps to arm oneself against a sea of .... ;;;;
;;;;;;;::;;;:;::;;;;::::;;:;::;;;;:::;;;::::;:;;:;::;;;:;::;:::;::;;;:;::;;;;;::;:;:;::;;:;
;;  		   	     
;;;;;;;;;;;;; CODE: ;;;;;;;;;;;;;
;;  		   	     

(define (make-weapon name location maxDmg)
  (let ((mobile-thing-part (make-mobile-thing name location)))
    (lambda (message)
      (case message
        ((WEAPON?) (lambda (self) #t))
        
        ((DAMAGE) (lambda (self) maxDmg))
        
        ((HIT) (lambda (self weaponOwner target)
                 (let ((dmg (random-number maxDmg)))
                   (ask target 'suffer dmg)
                   (ask self 'emit (list (ask weaponOwner 'name) "hit" dmg "damage to" (ask target 'name) "with" (ask self 'name))))))
        
        (else (get-method message mobile-thing-part))))))

(define (create-weapon name location maxDmg)
  (create make-weapon name location maxDmg))
  		   	     
;;;;;;;::;;;:;::;;;;::::;;:;::;;;;:::;;;::::;:;;:;::;;;:;::;:::;::;;;:;::;;;;;::;:;:;::;;::
;;;;;;;;;;;;; EXPLANATION: ;;;;;;;;;;;;;
;;  		   	     
  		   	     
; I have created the three method as requested. DAMAGE returns the max damage output.
; HIT makes the target suffer random amount of damage up to max damage. Also, HIT emits message about the details of the attack.		   	     	   	     


;;;;;;;::;;;:;::;;;;::::;;:;::;;;;:::;;;::::;:;;:;::;;;:;::;:::;::;;;:;::;;;;;::;:;:;::;:;;
;;;;;;;;;;;;; TRANSCRIPT: ;;;;;;;;;;;;;
;;

;(ask screen 'deity-mode #f)
;(setup 'baris)

;;;; value: ready

;(define katana (create-weapon 'lightsaber (ask me 'location) 7))
;(define dummy (create-autonomous-player 'dummy (ask me 'location) 1 1))
;(ask katana 'hit me dummy)

;;At migros dummy says -- Ouch! 6 hits is more than I want! 
;;At migros dummy says -- I have 2 more lives 
;;At migros baris hit 6 damage to dummy with lightsaber 

  		   	     
;;  		   	     
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;; END ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;::;;;:;::;;;;::::;;:;::;;;;:::;;;::::;:;;:;::;;;:;::;:::;::;;;:;::;;;;;::;:;:;::;:;:
  		   	     
  		   	     
  		   	     
  		   	     
  		   	     
  		   	     
  		   	     
  		   	     
  		   	     
  		   	     
;;;;;;;::;;;:;::;;;;::::;;:;::;;;;:::;;;::::;:;;:;::;;;:;::;:::;::;;;:;::;;;;;::;:;:;::;::;
;;;;;;;; Computer exercise 6: Good thing I'm armed and dangerous ;;;;;;;;;
;;;;;;;::;;;:;::;;;;::::;;:;::;;;;:::;;;::::;:;;:;::;;;:;::;:::;::;;;:;::;;;;;::;:;:;::;:::
;;  		   	     
;;;;;;;;;;;;; CODE: ;;;;;;;;;;;;;
;;

(define (make-violent-person name birthplace activity miserly attChance)
  (let ((autonomous-player-part (make-autonomous-player name birthplace activity miserly)))
    (lambda (message)
      (case message
        ((VIOLENT-PERSON?) (lambda (self) #t))
        
        ((INSTALL) (lambda (self)
                     (ask clock 'add-callback
                          (make-clock-callback 'try-attack self 'try-attack))
                     (delegate autonomous-player-part self 'install)))

        ((TRY-ATTACK)   		      	 
         (lambda (self)
           (let ((rndm (random-number attChance))
                 (targets (ask self 'PEOPLE-AROUND))
                 (weapons (myfilter (lambda (x) (is-a x 'weapon?)) (ask self 'things))))
             (if (and (= rndm 1) (not (null? targets)) (not (null? weapons)))
                 (let ((target (pick-random targets))
                       (weapon (pick-random weapons)))
                   (ask weapon 'hit self target))))))

        ((DIE)    	  	   	 
         (lambda (self)    	  	   	 
           (ask clock 'REMOVE-CALLBACK self 'CHECK-ENGAGE-IN-VIOLENT)
           (delegate autonomous-player-part self 'DIE)))

        (else (get-method message autonomous-player-part))))))

(define (create-violent-person name birthplace activity miserly attChance)
  (create make-violent-person name birthplace activity miserly attChance))
  		   	     
;;;;;;;::;;;:;::;;;;::::;;:;::;;;;:::;;;::::;:;;:;::;;;:;::;:::;::;;;:;::;;;;;::;:;:;:::;;;
;;;;;;;;;;;;; EXPLANATION: ;;;;;;;;;;;;;
;;  		   	     
  		   	     
; INSTALL provides that the object triggers TRY-ATTACK every tick. And as TRY-ATTACK triggers, with a 1 in attChance probability,
; the violent person hit random another person in the same location with the violent person, with a random weapon if there is any
; weapon and any person other than the violent person calling TRY-ATTACK method.
  		   	     
;;;;;;;::;;;:;::;;;;::::;;:;::;;;;:::;;;::::;:;;:;::;;;:;::;:::;::;;;:;::;;;;;::;:;:;:::;;:
;;;;;;;;;;;;; TRANSCRIPT: ;;;;;;;;;;;;;
;;

;(ask screen 'deity-mode #f)
;(setup 'baris)

;(define delucia (create-place 'delucia))
;(ask me 'change-location delucia)
;(define katana (create-weapon 'katana delucia 1))
;(define bow (create-weapon 'bow delucia 1))
;(define spear (create-weapon 'spear delucia 1))
;(define british (create-violent-person 'british delucia 1 2 2))
;(define blackthorn (create-violent-person 'blackthorn delucia 1 2 2))

;;;; value: ready


;(run-clock 5)

;;At delucia british says -- I take spear from delucia 
;;--- the-clock Tick 0 --- 
;;At delucia blackthorn says -- I take katana from delucia 
;;At delucia british says -- Ouch! 1 hits is more than I want! 
;;blackthorn hit 1 damage to british with katana 
;;--- the-clock Tick 1 --- 
;;At delucia blackthorn says -- I take bow from delucia 
;;At delucia blackthorn says -- Ouch! 1 hits is more than I want! 
;;british hit 1 damage to blackthorn with spear 
;;--- the-clock Tick 3 --- 
;;At delucia baris says -- Ouch! 1 hits is more than I want! 
;;blackthorn hit 1 damage to baris with spear 
;;At delucia british says -- I take spear from blackthorn 
;;At delucia blackthorn says -- I lose spear 
;;At delucia blackthorn says -- Yaaaah! I am upset! 
;;At delucia blackthorn says -- Ouch! 1 hits is more than I want! 
;;british hit 1 damage to blackthorn with spear 
;;--- the-clock Tick 4 --- 
;;At delucia blackthorn says -- I take katana from british 
;;;At delucia british says -- I lose katana 
;;At delucia british says -- Yaaaah! I am upset! 
;;At delucia baris says -- Ouch! 1 hits is more than I want! 
;;blackthorn hit 1 damage to baris with bow 
;;At delucia baris says -- Ouch! 1 hits is more than I want! 
;;At delucia baris says -- SHREEEEK!  I, uh, suddenly feel very faint...

;;;; value: done
  		   	     
;;  		   	     
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;; END ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;::;;;:;::;;;;::::;;:;::;;;;:::;;;::::;:;;:;::;;;:;::;:::;::;;;:;::;;;;;::;::;;::;;;;
  		   	     
  		   	     
  		   	     
  		   	     
  		   	     
  		   	     
  		   	     
  		   	     
;;;;;;;::;;;:;::;;;;::::;;:;::;;;;:::;;;::::;:;;:;::;;;:;::;:::;::;;;:;::;;;;;::;::;;::;;;:
;;; Computer exercise 7: A good hacker could defuse this situation ;;;;;;;
;;;;;;;::;;;:;::;;;;::::;;:;::;;;;:::;;;::::;:;;:;::;;;:;::;:::;::;;;:;::;;;;;::;::;;::;;:;
;;  		   	     
;;;;;;;;;;;;; CODE: ;;;;;;;;;;;;;
;;  		   	     

(define (make-bomb name location damage)
  (let ((mobile-thing-part (make-mobile-thing name location))
        (aware-thing-part (make-aware-thing))
        (isArmed #f)
        (isDestroyed #f))
    (lambda (message)
      (case message
        ((ARM) (lambda (self) (set! isArmed #t)))
        
        ((DISARM) (lambda (self) (set! isArmed #f)))

        ((IS-ARMED) (lambda (self) isArmed))
        
        ((TRIGGER) (lambda (self)
                     (if isArmed (ask self 'activate))))
        
        ((HEARD-NOISE) (lambda (self someone)
                         (ask self 'trigger)))

        ((DESTROY)
         (lambda (self)
           (set! isDestroyed #t)
           (delegate mobile-thing-part self 'DESTROY)))
        
        ((ACTIVATE)
         (lambda (self)
           (if (not isDestroyed)
               (begin
                 (map (lambda (x) (ask x 'suffer damage)) (ask self 'people-around))
                 (ask self 'emit (list (map (lambda (x) (ask x 'name)) (ask self 'people-around)) "got suffered" damage "damage from a" (ask self 'name)))
                 (ask self 'destroy)))))

        ((PEOPLE-AROUND)
         (lambda (self)  		   	     
           (let* ((in-room (ask (ask self 'LOCATION) 'THINGS))
                  (people (myfilter (lambda (x) (is-a x 'PERSON?)) in-room)))
             (delq self people))))

        (else (find-method message mobile-thing-part aware-thing-part))))))

(define (create-bomb name location damage)
  (create make-bomb name location damage))
  		   	     
;;;;;;;::;;;:;::;;;;::::;;:;::;;;;:::;;;::::;:;;:;::;;;:;::;:::;::;;;:;::;;;;;::;::;;::;;::
;;;;;;;;;;;;; EXPLANATION: ;;;;;;;;;;;;;
;;  		   	     
  		   	     
; A bomb get triggers from a HEARD-NOISE. A bomb cannot get TRIGGERed if it is not ARMed and it cannot be ACTIVATEd if it is not TRIGGERed.
; When a bomb ACTIVATEs, it makes the all people in the same location with the bomb suffers certain amount of damage.
; Also, I have added isDestroyed since DESTROY of the superclass did not work in a way that I want.		   	     
  		   	     
  		   	     
;;;;;;;::;;;:;::;;;;::::;;:;::;;;;:::;;;::::;:;;:;::;;;:;::;:::;::;;;:;::;;;;;::;::;;::;:;;
;;;;;;;;;;;;; TRANSCRIPT: ;;;;;;;;;;;;;
;;  		   	     

;(ask screen 'deity-mode #f)
;(setup 'baris)
;(define nuke (create-bomb 'nuke (ask me 'location) 100))
;(ask nuke 'arm)

;;;; value: ready

;(ask me 'look-around)

;;You are in great-court 
;;You are not holding anything. 
;;You see stuff in the room: nuke flag-pole 
;;There are no other people around you. 
;;The exits are in directions: up east west south north
;;;; value: ok

;(ask me 'go 'east)

;;baris moves from great-court to student-center 
;;--- the-clock Tick 0 --- 
;;You are in student-center 
;;You are not holding anything. 
;;There is no stuff in the room. 
;;There are no other people around you. 
;;The exits are in directions: in west
;;;; value: #t

;(ask me 'go 'west)

;;baris moves from student-center to great-court 
;;At great-court baris says -- Ouch! 100 hits is more than I want! 
;;At great-court baris says -- I have 2 more lives 
;;At great-court (baris)  got suffered  100  damage from a  nuke 
;;--- the-clock Tick 1 --- 
;;You are in great-court 
;;You are not holding anything. 
;;You see stuff in the room: flag-pole 
;;There are no other people around you. 
;;The exits are in directions: up east west south north
;;;; value: #t
  		   	     
;;  		   	     
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;; END ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;::;;;:;::;;;;::::;;:;::;;;;:::;;;::::;:;;:;::;;;:;::;:::;::;;;:;::;;;;;::;::;;::;:;:
  		   	     
  		   	     
  		   	     
  		   	     
  		   	     
  		   	     
  		   	     
  		   	     
;;;;;;;::;;;:;::;;;;::::;;:;::;;;;:::;;;::::;:;;:;::;;;:;::;:::;::;;;:;::;;;;;::;::;;::;::;
;;;; Computer exercise 8: Well, maybe only if they have enough time ;;;;;;
;;;;;;;::;;;:;::;;;;::::;;:;::;;;;:::;;;::::;:;;:;::;;;:;::;:::;::;;;:;::;;;;;::;::;;::;:::
;;  		   	     
;;;;;;;;;;;;; CODE: ;;;;;;;;;;;;;
;;  		

(define (make-bomb-with-timer name location damage)
  (let ((bomb-part (make-bomb name location damage))
        (timeLeft 3)
        (isTriggered #f))
    (lambda (message)
      (case message
        ((INSTALL) (lambda (self)
                     (ask clock 'add-callback
                          (make-clock-callback 'tick self 'tick))
                     (delegate bomb-part self 'install)))

        ((TICK)
         (lambda (self)
           (if (and isTriggered (= timeLeft 1))
                   (ask self 'activate)
                   (begin
                     (set! timeLeft (- timeLeft 1))
                     (ask self 'emit (list timeLeft "seconds left to explosion"))))))

        ((TRIGGER) (lambda (self)
                     (if (ask self 'is-armed)
                         (set! isTriggered #t))))
                    
        (else (get-method message bomb-part))))))

(define (create-bomb-with-timer name location damage)
  (create make-bomb-with-timer name location damage))
  		   	     
;;;;;;;::;;;:;::;;;;::::;;:;::;;;;:::;;;::::;:;;:;::;;;:;::;:::;::;;;:;::;;;;;::;::;;:::;;;
;;;;;;;;;;;;; EXPLANATION: ;;;;;;;;;;;;;
;;  		   	     
  		   	     
; At first, there was an extra parameter which defines the time of the bomb. But in the setup.scm there is not any extra parameter for create-bomb-with-timer
; so I have set it to the fixed value which is 3 seconds. Every TICK, timeLeft decreases 1 and when it is equal to 1, it sends ACTIVATE method to super class.
  		   	     
  		   	     
;;;;;;;::;;;:;::;;;;::::;;:;::;;;;:::;;;::::;:;;:;::;;;:;::;:::;::;;;:;::;;;;;::;::;;:::;;:
;;;;;;;;;;;;; TRANSCRIPT: ;;;;;;;;;;;;;
;;  		   	     

;(ask screen 'deity-mode #f)
;(setup 'baris)
;(define sound-bomb (create-bomb-with-timer 'sound-bomb (ask me 'location) 1))
;(ask sound-bomb 'arm)

;;;; value: ready

;(ask me 'go 'east)

;;baris moves from bursar-office to registrar-office 
;;--- the-clock Tick 0 --- 
;;You are in registrar-office 
;;You are not holding anything. 
;;You see stuff in the room: diploma 
;;There are no other people around you. 
;;The exits are in directions: west out
;;;; value: #t

;(ask me 'go 'west)

;;baris moves from registrar-office to bursar-office 
;;At bursar-office 2 seconds left to explosion 
;;--- the-clock Tick 1 --- 
;;You are in bursar-office 
;;You are not holding anything. 
;;You see stuff in the room: sound-bomb 
;;There are no other people around you. 
;;The exits are in directions: east
;;;; value: #t

;(run-clock 2)

;;At bursar-office 1 seconds left to explosion 
;;--- the-clock Tick 2 --- 
;;At bursar-office baris says -- Ouch! 1 hits is more than I want! 
;;At bursar-office (baris)  got suffered  1  damage from a  sound-bomb 
;;;; value: done		   	     
  		   	     
;;  		   	     
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;; END ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;::;;;:;::;;;;::::;;:;::;;;;:::;;;::::;:;;:;::;;;:;::;:::;::;;;:;::;;;;;::;:::;::;;;;
  		   	     
  		   	     
  		   	     
  		   	     
  		   	     
  		   	     
  		   	     
  		   	     
  		   	     
;;;;;;;::;;;:;::;;;;::::;;:;::;;;;:::;;;::::;:;;:;::;;;:;::;:::;::;;;:;::;;;;;::;:::;::;;;:
;;;;;;;;; Computer Exercise 9: Even you can change the world! ;;;;;;;;;;;;
;;;;;;;::;;;:;::;;;;::::;;:;::;;;;:::;;;::::;:;;:;::;;;:;::;:::;::;;;:;::;;;;;::;:::;::;;:;
  		   	     
  		   	     
;;;;;;;::;;;:;::;;;;::::;;:;::;;;;:::;;;::::;:;;:;::;;;:;::;:::;::;;;:;::;;;;;::;:::;::;;::
;;;;;;;;;;;;; DESCRIPTION: ;;;;;;;;;;;;;
;;  		   	     

; I will create a bomberman class which is a subclass of autonomous-player. Bomberman plants bombs with a random chance.  		   	     
  		   	     
;;;;;;;::;;;:;::;;;;::::;;:;::;;;;:::;;;::::;:;;:;::;;;:;::;:::;::;;;:;::;;;;;::;:::;::;:;;
;;;;;;;;;;;;; CODE: ;;;;;;;;;;;;;
;;  		   	     

(define (make-bomberman name birthplace activity miserly bombChance)
  (let ((autonomous-player-part (make-autonomous-player name birthplace activity miserly)))
    (lambda (message)
      (case message
        ((BOMBERMAN?) (lambda (self) #t))
        
        ((INSTALL) (lambda (self)
                     (ask clock 'add-callback
                          (make-clock-callback 'try-plant-bomb self 'try-plant-bomb))
                     (delegate autonomous-player-part self 'install)))

        ((TRY-PLANT-BOMB)
         (lambda (self)
           (let ((rndm (random-number bombChance)))
             (if (= rndm 1)
                 (let ((bomb (create-bomb-with-timer 'time-bomb (ask self 'location) 5)))
                   (begin
                     (ask bomb 'arm)
                     (ask self 'emit (list "a bomb has been planted to " (ask (ask self 'location) 'name)))))))))

        ((DIE)    	  	   	 
         (lambda (self)    	  	   	 
           (ask clock 'REMOVE-CALLBACK self 'CHECK-ENGAGE-IN-VIOLENT)
           (delegate autonomous-player-part self 'DIE)))

        (else (get-method message autonomous-player-part))))))

(define (create-bomberman name birthplace activity miserly bombChance)
  (create make-bomberman name birthplace activity miserly bombChance))
  		   	     
  		   	     
;;;;;;;::;;;:;::;;;;::::;;:;::;;;;:::;;;::::;:;;:;::;;;:;::;:::;::;;;:;::;;;;;::;:::;::;:;:
;;;;;;;;;;;;; EXPLANATION: ;;;;;;;;;;;;;
;;  		   	     
  		   	     
; Bomberman travels across the map just like the autonomous-player. As the bomberman travels between locations,
; bomberman plants a bomb with a 1 in bombChance probability to the location where the bomberman existing in. 	     
  		   	     
  		   	     
;;;;;;;::;;;:;::;;;;::::;;:;::;;;;:::;;;::::;:;;:;::;;;:;::;:::;::;;;:;::;;;;;::;:::;::;::;
;;;;;;;;;;;;; TRANSCRIPT: ;;;;;;;;;;;;;
;;  		   	     

;(ask screen 'deity-mode #t)
;(setup 'baris)
;(define bomberman1 (create-bomberman 'bomberman1 (ask me 'location) 2 2 1))
;(define bomberman2 (create-bomberman 'bomberman1 (ask me 'location) 2 2 1))

;;;; value: ready

;(run-clock 10)

;;bomberman1 moves from suzy-cafe to cici-bufe 
;;At cici-bufe bomberman1 says -- Hi cici 
;;bomberman1 moves from cici-bufe to divan 
;;At divan bomberman1 says -- I take profiterol from divan 
;;At divan a bomb has been planted to  divan 
;;bomberman1 moves from cici-bufe to migros 
;;At migros a bomb has been planted to  migros 
;;--- the-clock Tick 0 ---

;;--- the-clock Tick 3 --- 
;;At suzy-cafe 1 seconds left to explosion 
;;At cici-bufe cici says -- Ouch! 5 hits is more than I want! 
;;At cici-bufe cici says -- I have 2 more lives 
;;At cici-bufe (cici) got suffered 5 damage from a time-bomb 
;;--- the-clock Tick 4 ---
;;At suzy-cafe bomberman1 says -- Ouch! 5 hits is more than I want! 
;;At suzy-cafe bomberman1 says -- SHREEEEK!  I, uh, suddenly feel very faint... 
;;An earth-shattering, soul-piercing scream is heard... 
;;bomberman1 moves from suzy-cafe to heaven 
;;At suzy-cafe bomberman1 says -- Ouch! 5 hits is more than I want! 
;;At suzy-cafe bomberman1 says -- I have 1 more lives 
;;At suzy-cafe baris says -- Ouch! 5 hits is more than I want! 
;;At suzy-cafe baris says -- I have 2 more lives 
;;At suzy-cafe (baris bomberman1) got suffered 5 damage from a time-bomb
;;--- the-clock Tick 5 ---

;;;; value: done

  		   	     
;;  		   	     
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;; END ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;::;;;:;::;;;;::::;;:;::;;;;:::;;;::::;:;;:;::;;;:;::;:::;::;;;:;::;;;;;::;:::;::;:::
  		   	     
  		   	     
  		   	     
  		   	     
  		   	     
;;;;;;;::;;;:;::;;;;::::;;:;::;;;;:::;;;::::;:;;:;::;;;:;::;:::;::;;;:;::;;;;;::;:::;:::;;;
;# DO NOT FORGET TO SUBMIT YOUR WORK BEFORE THE DEADLINE!         #
;;;;;;;::;;;:;::;;;;::::;;:;::;;;;:::;;;::::;:;;:;::;;;:;::;:::;::;;;:;::;;;;;::;:::;:::;;:
;# GOOD LUCK!                                                     #
;;;;;;;::;;;:;::;;;;::::;;:;::;;;;:::;;;::::;:;;:;::;;;:;::;:::;::;;;:;::;;;;;:::;;;;::;;;;
  		   	     
  		   	     
  		   	     
  		   	     
  		   	     
  		   	     
