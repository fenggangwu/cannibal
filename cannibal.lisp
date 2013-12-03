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

; heuristic function generator
; boat-capacity:
; return: a heuristic function with respect to the boat capacity
(defun h-cannibal-gen (boat-capacity)
  (lambda (state goal)
    (/ (+ (- (car state) (car goal)) 
	  (- (cadr state) (cadr goal))) 
       boat-capacity)))


(defun h (state goal) 0)

(defun h-cannibal (state goal)
  (/ (+ (- (car state) (car goal) ) (- (cadr state) (cadr goal) )) 6))

(defun solve-cannibal (people-num boat-capacity)
  (astar (list people-num people-num 1) 
	 '(0 0 0)
	 (generate-moves-max boat-capacity people-num)
	 (h-cannibal-gen boat-capacity)))

;(run-depth '(21 21 1) '(0 0 0) (generate-moves-max 6 21))
;(run-depth '(28 28 1) '(0 0 0) (generate-moves-max 6 28))


;(astar '(21 21 1) '(0 0 0) (generate-moves-max 6 21) (h-cannibal-gen 6))
;(astar '(28 28 1) '(0 0 0) (generate-moves-max 6 28) (h-cannibal-gen 6))

;(solve-cannibal 3 2)
;(solve-cannibal 26 6)
;(solve-cannibal 28 6)
