;;; This is one of the example programs from the textbook:
;;;
;;; Artificial Intelligence: 
;;; Structures and strategies for complex problem solving
;;;
;;; by George F. Luger and William A. Stubblefield
;;;
;;; Corrections by Christopher E. Davis (chris2d@cs.unm.edu)
;;; 
;;; 
;;; These programs are copyrighted by Benjamin/Cummings Publishers.
;;;
;;; We offer them for use, free of charge, for educational purposes only.
;;;
;;; Disclaimer: These programs are provided with no warranty whatsoever as to
;;; their correctness, reliability, or any other property.  We have written 
;;; them for specific educational purposes, and have made no effort
;;; to produce commercial quality computer programs.  Please do not expect 
;;; more of them then we have intended.
;;;

;;;
;;; This code has been tested with CMU Common Lisp CVS release-19a
;;; 19a-release-20040728 and appears to function as intended.

;;; This file contains the depth first search algorithm from chapter 7.

; This version of depth first search does not use open and closed lists
; to keep track of states.  Instead, it uses recursion to manage the search.

;;; It takes as arguments a start state, a goal state, and a list of
;;; move functions.

;;; For example, to run depth first search with the farmer, wolf, 
;;; goat, etc. problem, evaluate the definitions found in the file 
;;; farmer_wolf_etc_rules_only, and evaluate:
;
;     (run-depth-first '(e e e e) '(w w w w)
;   '(farmer-takes-self farmer-takes-wolf 
;     farmer-takes-goat farmer-takes-cabbage))
;


(defun depth-first-search (start goal been-list moves)
  (cond ((equal start goal) 
         (reverse (cons start been-list)))
        (t (try-moves start goal been-list moves moves))))

; Try-moves scans down the list of moves in moves-to-try, 
; attempting to generate a child state.  If it produces 
; this state, it calls depth-first-search to complete the search.

(defun try-moves (start goal been-list moves-to-try moves)
  (cond ((null moves-to-try) nil)
        ((member start been-list :test #'equal) nil)
        (t (let ((child (funcall (car moves-to-try) start)))
             (if child 
               (or (depth-first-search (funcall (car moves-to-try) start)
                                       goal
                                       (cons start been-list)
                                       moves)
                   (try-moves start goal been-list (cdr moves-to-try) moves))
               (try-moves start goal been-list (cdr moves-to-try) moves))))))

; run-depth-first calls depth-first-search, initializing the been-list to ().
(defun run-depth (start goal moves)
  (depth-first-search start goal () moves))

; state: (M, C, B)
; M: missionary number in this bank
; C: cannibal number in this bank
; B: boat number in this bank

; create the state-moving function
; m: number of missionaries to be moved by boat
; c: number of cannibals to be moved by boat
; n: total number of missionaries or cannibals on both bank
; returns: a function that can move one state to another.
(defun create-move (m c n)
  (lambda (state) 
    (if (> (caddr state) 0) ; boat is on this bank. So we deduce number of people of this bank
	(cond
	  ((or (< m 0) (< c 0) (and (= m 0) (= c 0))) ;(format t "there must be non-negtive number of people; Empty boat cannot sail")
	   nil)
	  ((and (> c m) (> m 0)) ;(format t "cannibals cannot outnumber missionaries on boat") 
	   nil)
	  ((< (- (car state) m) 0);(format t "missionaris number on this bank must be postive") 
	   nil) 
	  ((< (- (cadr state) c) 0) ;(format t"cannibals number on this bank must be postive") 
	   nil) 
	  ((and 
	    (> (- (cadr state) c) (- (car state) m)) 
	    (> (- (car state) m) 0))
	   ;(format t "cannibals cannot outnumber missional on this bank")
	   nil) 
	  ((and
	    (> (- n (- (cadr state) c)) (- n (- (car state) m))) 
	    (> (- n (- (car state) m)) 0))
	   ;(format t "cannibals cannot outnumber missional on other bank")
	   nil) 
	  (t (list (- (car state) m) (- (cadr state) c) 0))
	  )
	(cond ; boat is on other bank. So we deduce number of people on the other bank
	  ((or (< m 0) (< c 0) (and (= m 0) (= c 0))) ;(format t "there must be non-negtive number of people; Empty boat cannot sail")
	   nil); 
	  ((and (> c m) (> m 0)) ;(format t "cannibals cannot outnumber missionaries on boat" ) 
	   nil)
	  ((< (- n (car state) m) 0) ;(format t "missionaris number on other bank must be postive") 
	   nil) ;
	  ((< (- n (cadr state) c) 0) ;(format t "cannibals number on other bank must be postive") 
	   nil) ;
	  ((and 
	    (> (+ (cadr state) c) (+ (car state) m)) 
	    (> (+ (car state) m) 0))
	   ;(format t "cannibals cannot outnumber missional on this bank") 
	   nil) ;
	  ((and
	    (> (- n (cadr state) c) (- n  (car state) m)) 
	    (> (- n (car state) m) 0))
	   ;(format t "cannibals cannot outnumber missional on other bank")
	   nil) 
	  (t (list (+ (car state) m) (+ (cadr state) c) 1))))))

	  
; generate legal list of possible move functions.
; maxp: max number of people on the boat
(defun generate-moves-max (maxp n)
  (do
   ((j maxp (- j 1))(move-list nil (append move-list (generate-moves j n))))
   ((< j 1) move-list)))

; helper function
(defun generate-moves (p n)
  (do
   ((i 0 (+ i 1)) 
    ;(move-list nil (cons (list i (- p i))  move-list)))
    (move-list nil (cons (create-move i (- p i) n) move-list)))
   ((> i p) move-list)))

;(run-depth '(21 21 1) '(0 0 0) (generate-moves-max 6 21))
;(run-depth '(28 28 1) '(0 0 0) (generate-moves-max 6 28))
