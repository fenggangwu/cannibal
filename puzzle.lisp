
; the 4 possible moves between states
; return the state after move, or nil if the move is illegal
(defun move-down (state)
  (let ((copy-state (deep-copy state)))
    (cond ((member 'E (car copy-state)) 
	   (let ((pos (position 'E (car copy-state))))
	      (setf (nth pos (car copy-state)) (nth pos (cadr copy-state)))
	      (setf (nth pos (cadr copy-state)) 'E))
	    copy-state)  
	  ((member 'E (cadr copy-state)) 
	   (let ((pos (position 'E (cadr copy-state))))
	     (setf (nth pos (cadr copy-state)) (nth pos (caddr copy-state)))
	     (setf (nth pos (caddr copy-state)) 'E))
	    copy-state)
	  (t nil))))

(defun move-up (state)
  (let ((copy-state (deep-copy state)))
    (cond ((member 'E (car copy-state)) nil)
	  ((member 'E (cadr copy-state))
	   (let ((pos (position 'E (cadr copy-state))))
	     (setf (nth pos (cadr copy-state)) (nth pos (car copy-state)))
	     (setf (nth pos (car copy-state)) 'E))
	   copy-state)
	  ((member 'E (caddr copy-state))
	   (let ((pos (position 'E (caddr copy-state))))
	     (setf (nth pos (caddr copy-state)) (nth pos (cadr copy-state)))
	     (setf (nth pos (cadr copy-state)) 'E))
	   copy-state))))

(defun move-right (state)
  (let ((copy-state (deep-copy state)))
    (cond ((member 'E (car copy-state))
	   (let ((pos (position 'E (car copy-state))))
	     (cond ((= pos 0)
		    (setf (caar copy-state) (cadar copy-state))
		    (setf (cadar copy-state) 'E)
		    copy-state)
		   ((= pos 1)
		    (setf (cadar copy-state) (caddar copy-state))
		    (setf (caddar copy-state) 'E)
		    copy-state)
		   (t nil))))
	  ((member 'E (cadr copy-state))
	   (let ((pos (position 'E (cadr copy-state))))
	     (cond ((= pos 0)
		    (setf (caadr copy-state) (cadadr copy-state))
		    (setf (cadadr copy-state) 'E)
		    copy-state)
		   ((= pos 1)
		    (setf (cadr (cadr copy-state)) (caddr (cadr copy-state)))
		    (setf (caddr (cadr copy-state)) 'E)
		    copy-state)
		   (t nil))))
	  (t
	   (let ((pos (position 'E (caddr copy-state))))
	     (cond ((= pos 0)
		    (setf (car (caddr copy-state)) (cadr (caddr copy-state)))
		    (setf (cadr (caddr copy-state)) 'E)
		    copy-state)
		   ((= pos 1)
		    (setf (cadr (caddr copy-state)) (caddr (caddr copy-state)))
		    (setf (caddr (caddr copy-state)) 'E)
		    copy-state)
		   (t nil)))))))


(defun move-left (state)
  (let ((copy-state (deep-copy state)))
    (cond ((member 'E (car copy-state))
	   (let ((pos (position 'E (car copy-state))))
	     (cond ((= pos 0) nil)
		   ((= pos 1)
		    (setf (cadar copy-state) (caar copy-state))
		    (setf (caar copy-state) 'E) 
		    copy-state)
		   (t
		    (setf (caddar copy-state) (cadar copy-state))
		    (setf (cadar copy-state) 'E) 
		    copy-state))))
	  ((member 'E (cadr copy-state))
	   (let ((pos (position 'E (cadr copy-state))))
	     (cond ((= pos 0) nil)
		   ((= pos 1)
		    (setf (cadadr copy-state) (caadr copy-state))
		    (setf (caadr copy-state) 'E) 
		    copy-state)
		   (t
		    (setf (caddr (cadr copy-state)) (cadr (cadr copy-state)))
		    (setf (cadr (cadr copy-state)) 'E)
		    copy-state))))
	  (t
	   (let ((pos (position 'E (caddr copy-state))))
	     (cond ((= pos 0) nil)
		   ((= pos 1)
		    (setf (cadr (caddr copy-state)) (car (caddr copy-state)))
		    (setf (car (caddr copy-state)) 'E)
		    copy-state)
		   (t
		    (setf (caddr (caddr copy-state))
			   (cadr (caddr copy-state)))
		    (setf (cadr (caddr copy-state)) 'E)
		    copy-state)))))))



(defun h-8puzzle (state goal)
  (compare-matrix state goal)
)

(defun compare-matrix (m1 m2)
  (cond ((not (equal (list-length m1) (list-length m2))) nil)
	((null m1) 0)
	(t (+ (compare-row (car m1) (car m2))
	      (compare-matrix (cdr m1) (cdr m2))))))

(defun compare-row (r1 r2)
  (cond ((not (equal (list-length r1) (list-length r2))) nil)
	((null r1) 0)
	((equal (car r1) (car r2)) (compare-row (cdr r1) (cdr r2)))
	(t (+ 1 (compare-row (cdr r1) (cdr r2))))))

;deep-copy a state
(defun deep-copy (state)
  (list (copy-list (car state)) 
	(copy-list (cadr state)) 
	(copy-list (caddr state))))


(defun solve-puzzle (start)
  (if (detect-infeasible start)
      (format t "Infeasible puzzle")
      (astar start 
	     '((1 2 3) (4 5 6) (7 8 E)) 
	     (list 'move-up 'move-down 'move-left 'move-right)
	     'h-8puzzle)))

; reference: http://www.cs.bham.ac.uk/~mdr/teaching/modules04/java2/TilesSolvability.html
(defun detect-infeasible (start)
  (oddp (count-list-inverse (remove 'E (append (car start) (cadr start) (caddr start))))))

(defun count-list-inverse (l)
  (if (null l)
      0
      (+ (count-inverse (car l) (cdr l))
	 (count-list-inverse (cdr l)))))

(defun count-inverse (e l)
  (cond ((null l) 0)
	((> e (car l)) (+ 1 (count-inverse e (cdr l))))
	(t (count-inverse e (cdr l)))))


; (astar '((E 1 3)(4 2 5)(7 8 6)) '((1 2 3)(4 5 6)(7 8 E)) (list 'move-up 'move-down 'move-left 'move-right) 'h-8puzzle)


; (solve-puzzle '(( 1 6 3)(8 E 5)(4 2 7)))
; (solve-puzzle '((4 2 3) (5 E 1) (8 6 7)))

