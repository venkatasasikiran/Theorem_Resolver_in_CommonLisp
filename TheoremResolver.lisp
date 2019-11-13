;;;Venkata Sasikiran Veeramachaneni 

(defun resolution(lc)
;;lc is atleast two proportional clauses representing
;;a set of axioms and the negation of the theorem to prove
  (let ((app-list '()))	
	(setf app-list (depth-first-strategy lc lc))
	;performs depth first search and returns new resolvents
	(cond  ((member '(nil nil) app-list :test #'equal)  (print "A contradiction had been found")  'NIL)
		   ((null app-list)  			 (print "There is no contradiction in the given set of clauses")  'T)
		   (T							 (resolution (append app-list lc))))))
		   

(defun depth-first-strategy(l lo)
;;performs depth first on the list l
;;l is the list of clauses 
;;lo is the original list of clauses passed initially and preserved through recursion
  (let ((perform-strategy 'T) (app-list '()) (temp '()))
	(if (null (rest l)) (setf perform-strategy 'NIL))
	(when perform-strategy
		(dolist (x (rest l) app-list)
				(setf temp (resolve (first l) x))
				(if temp (if (not (member temp lo :test #'equal))(setf app-list (cons temp app-list))))))
	(cond  ((not perform-strategy)  '())
		   ((null app-list)			(depth-first-strategy (rest l) lo))
		   ( T						app-list))))



(defun resolve(clause1 clause2)
;;resolves clause1 and clause2
  (let ((lr-way nil)(rl-way nil) (resolvent-lr nil) (resolvent-rl nil) (temp1 nil) (temp2 nil))	
	(setf temp1 (resolve-initial (first clause1) (second clause2)))
	(setf lr-way (second temp1))
	(setf resolvent-lr (first temp1))
	(setf temp2 (resolve-initial (first clause2) (second clause1)))
	(setf rl-way (second temp2))
	(setf resolvent-rl (first temp2))	
	(cond  ((and lr-way rl-way) )
		(lr-way  (resolve-final resolvent-lr (list (first clause2) (second clause1))))
		(rl-way  (resolve-final resolvent-rl (list (first clause1) (second clause2)))))))
		   

		   
(defun resolve-initial(l1 l2)
;;l1 is either left side of clause1 or clause2
;;l2 is either right side of clause1 or clause2
;;finds and cancels first common element in l1 and l2
  (let ((temp nil))
  (cond ((or (null l1) (null l2))	(list (list l1 l2) NIL))
		((member (first l1) l2)		(list (list (remove (first l1) l1) (remove (first l1) l2)) T))
		(T							(setf temp	(resolve-initial (rest l1) l2))
									(list (list (cons (first l1) (first (first temp))) (second (first temp))) (second temp))))))
  
(defun resolve-final(l1 l2)
;;l1 and l2 are clauses
;;finds union of left sides and right sides of l1 and l2
;;returns a signle clause
  (list (merge-side (first l1) (first l2)) (merge-side (second l1) (second l2))))
  
(defun merge-side(fl1 fl2)
;;fl1 and fl2 are either left hand sides or right hand sides of two clauses
;;finds the union of fl1 and fl2
  (cond ((null fl1)				fl2)
		  ((member (first fl1) fl2)	(merge-side (rest fl1) fl2))
		  (T						(cons (first fl1) (merge-side (rest fl1) fl2)))))

(defun test-resolution ()
;; testing function for resolution
(print '((()(A)) ((A)())))
(print (resolution '((()(A)) ((A)()))))
(terpri)
(print '((()(A)) (()(A))))
(print (resolution '((()(A)) (()(A)))))
(terpri)
(print '(((B)(A)) ((A)(B))))
(print (resolution '(((B)(A)) ((A)(B)))))
(terpri)
(print '(((P)(Q)) ((Q)(R)) ((R)(S))))
(print (resolution '(((P)(Q)) ((Q)(R)) ((R)(S)))))
(terpri)
(print '(((X)(Y)) ((P)(Q)) ((M)(N))))
(print (resolution '(((X)(Y)) ((P)(Q)) ((M)(N)))))
(terpri)
(print '( ((C D)(A)) ((A D E)()) (()(A C)) (()(D)) (()(E)) ))
(print (resolution '( ((C D)(A)) ((A D E)()) (()(A C)) (()(D)) (()(E)) )))
(terpri)
(print '( ((A)(B C)) ((B)()) (()(A)) ((C)()) )) 
(print (resolution '( ((A)(B C)) ((B)()) (()(A)) ((C)()) )))
(terpri)
(print '( ((A)(B C)) ((B)()) ((C)()) ))
(print (resolution '( ((A)(B C)) ((B)()) ((C)()) ))))


;;Output of the testing session

;((NIL (A)) ((A) NIL))
;"A contradiction had been found"
;NIL

;((NIL (A)) (NIL (A)))
;"There is no contradiction in the given set of clauses"
;T

;(((B) (A)) ((A) (B)))
;"There is no contradiction in the given set of clauses"
;T

;(((P) (Q)) ((Q) (R)) ((R) (S)))
;"There is no contradiction in the given set of clauses"
;T

;(((X) (Y)) ((P) (Q)) ((M) (N)))
;"There is no contradiction in the given set of clauses"
;T

;(((C D) (A)) ((A D E) NIL) (NIL (A C)) (NIL (D)) (NIL (E)))
;"A contradiction had been found"
;NIL

;(((A) (B C)) ((B) NIL) (NIL (A)) ((C) NIL))
;"A contradiction had been found"
;NIL

;(((A) (B C)) ((B) NIL) ((C) NIL))
;"There is no contradiction in the given set of clauses"
;T
;T
