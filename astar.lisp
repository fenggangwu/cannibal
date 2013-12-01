
; structure usage => instead we use pure list to process
; a node will be a list that has the following format:
; node: (state f-value g-value path-from-start-to-here)


; priority queue => we can use sort key and function


; visited-node vs been-list
; we can explore to visited nodes. But there is no point to revisited
; a node that is on the backwards path.
; 
; new problem: repeatly expand the same state from different path is 
; a waste of time => solution, replace the same state with the one 
; having lower cost

(defun astar (start goal moves h)
;  (print moves)
  (astar-search (list (make-node start goal nil 0 h)) ; the frontier list
		goal moves h))


(defun astar-search (frontier goal moves h) ;todo
;  (format t "frontier ~D" (list-length froniter))
;  (format t "~{~a~^, ~}" frontier)
;  (print moves)
  (cond ((checkgoal frontier goal) 
	 (reverse (cadddr (checkgoal frontier goal))))
	(t (astar-search (expand-best-cand frontier goal moves h)
			 goal moves h))))

;eliminate duplicate
;reorder the nodes
;expand the best candidate to see whether goal reaches



; expand the best frontier to get new froniter
; assume the old frontier is sorted already
; frontier: the old frontier (sorted increasingly by f-value)
; returns: the new frontier (sorted increasingly by f-value)
(defun expand-best-cand (frontier goal moves h)
  (let ((new-frontier 
	 (my-append (expand (car frontier) 
			    goal
			    moves 
			    (cadddr (car frontier)); path of the best cand
			    (+ (caddar frontier) 1) ;g-value+1 for now node
			    h) 
		    (cdr frontier)))) 
;    (print (cadr frontier))
;    (print (list-length new-frontier))
    (format t "expanding ~D (f=~D) ~% ~D nodes expanded, ~D nodes in frontier  ~%~%" 
	    (caar frontier) 
	    (cadar frontier) 
	    (list-length (expand (car frontier) 
			    goal
			    moves 
			    (cadddr (car frontier)); path of the best cand
			    (+ (caddar frontier) 1) ;g-value+1 for now node
			    h))
	    (list-length new-frontier))
    (sort new-frontier #'< :key #'second)))

; expand node, collect all the children nodes in a list
; eliminating the nodes that is already on its own path
; parents-path: the path of the node to be expanded
; returns: a list containing all the children
(defun expand (node goal moves parents-path g-value h)
  (if (null moves)
      nil
      (let ((new-state (funcall (car moves) (car node)) ) )
	(cond ((not new-state) ; if we reach to a invalid point
	       (expand node goal (cdr moves) 
		       parents-path g-value h ))
	      ((member new-state (cadddr node) :test #'equal);if already in path
	       (expand node goal (cdr moves) 
		       parents-path g-value h ))
	      (t (cons  ;if valid
		  (make-node (funcall (car moves) (car node)) 
			     goal parents-path g-value h) 
		  (expand node goal (cdr moves)
			  parents-path g-value h )))))))


(defun make-node (state goal parents-path g-value h)
  (list state 
	(+ (funcall h state goal) g-value) ;new f-value
	g-value ;new g-value
	(cons state parents-path)));new path, containing the state itself


;check whether the frontier reach the goal?
(defun checkgoal (frontier goal)
  (cond ((null frontier) nil)
	((equal (caar frontier) goal) (car frontier))
	(t (checkgoal (cdr frontier) goal))))

(defun h (state goal) 0)

(defun h-cannibal (state goal)
(/ (+ (- (car state) (car goal) ) (- (cadr state) (cadr goal) )) 6))

; appending the newly expanded node to the existing frontier
; if two nodes are having the same state, keep the one having lower cost
(defun my-append (new-nodes frontier)
  (cond ((null new-nodes) frontier)
	(t
;	 (print (cdr new-nodes))
;	 (print (insert-node (car new-nodes) frontier))
	 (my-append 
	  (cdr new-nodes) 
	  (insert-node (car new-nodes) frontier)))))
; non-destroy function
; insert node to frontier
; if the new node have the same state with an old one, only keep the 
; one having the lower cost (f-value)
(defun insert-node (node frontier)
  (cond ((null frontier) (list node))
	((equal (car node) (caar frontier)) 
	 (if (< (cadr node) (cadar frontier))
	     (cons node (cdr frontier))
	     frontier))
	(t (cons (car frontier) (insert-node node (cdr frontier))))))


; heuristic function generator
; boat-capacity:
; return: a heuristic function with respect to the boat capacity
(defun h-cannibal-gen (boat-capacity)
  (lambda (state goal)
    (/ (+ (- (car state) (car goal)) 
	  (- (cadr state) (cadr goal))) 
       boat-capacity)))