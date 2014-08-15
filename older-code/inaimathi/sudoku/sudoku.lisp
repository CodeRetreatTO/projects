(defpackage :sudoku (:use :cl))
(in-package :sudoku)

;; a solution is a board where
;;;; each row is a set
;;;; each column is a set
;;;; each disjoint 3x3 section is a set

;; generating
;;;; -pick row/column/3x3
;;;; -generate a random set
;;;; -expand according to sudoku rules
;;;; -redact all but n (between 3 and 6 numbers)

;; solving
;;;; -start with valid question board
;;;; -if there is a set with 1 remaining element, fill it in and continue with the new board
;;;; -if there is a square with only one possibility, fill it in and continue with the new board
;;;; -otherwise get the set with the fewest number of possibilities and try each of them.
;;;;;; -if multiples result in a solution, pick one.

;;; Indexing bug happening. Hunt that down and try again. I think the principle is sound though.

(defun solve-obvious! (board)
  (loop for changed = nil
     do (loop for x from 0 repeat 9
	   do (loop for y from 0 repeat 9
		 for ps = (possibilities board x y)
		 do (when (and ps (empty? board x y) (not (second ps)))
		      (break "~a~%~a, ~a => ~a" board x y (first ps))
		      (setf (aref board y x) (first ps)
			    changed t))))
     while changed))

(defun solve (board)
  (case (board-triage board)
    (:full board)
    (:impossible nil)
    (:empties 
     ;; find next empty, `solve` on each possibility
     ))
  (cond ((not (any-empty? solution)) board)
	()))

(defun generate-board (&optional (fill 5))
  (let ((board (empty-board)))
    (flet ((gen () (random 9)))
      (loop with filled = 0
	 for x = (gen) and y = (gen)
	 do (when (and (empty? board x y) (possibilities board x y))
	      (let ((val (pick (possibilities board x y))))
		(break "~a~%~a, ~a :: ~a~%~a => ~a" 
		       board x y (aref board y x) (possibilities board x y) val)
		(setf (aref board y x) val)
		(incf filled)))
	 until (= filled fill)))
    board))

;;;;;;;;;; Sudoku-specific utility
(defun possibilities (board x y)
  (reduce #'set-difference
	  (list (row board y)
		(column board x)
		(square board x y))
	  :initial-value (range 1 9)))

(defun row (board y)
  (loop for i from 0 repeat 9
     collect (aref board y i)))

(defun column (board x)
  (loop for i from 0 repeat 9
     collect (aref board i x)))

(defun square (board x y)
  (flet ((origin (n) (* 3 (floor n 3))))
    (loop repeat 3 for i from (origin y)
       append (loop repeat 3 for j from (origin x) 
		 collect (aref board i j)))))

(defun empty-board ()
  (make-array (list 9 9) :initial-element '_))

(defun board-triage (board)
  "Returns :empties :impossible or :full"
  (loop for x from 0 repeat 9
     do (loop for y 0 repeat 9
	   do (cond ((and (empty? board x y) (null (possibilities board x y)))
		     (return :impossible))
		    ((empty? board x y)
		     (return :empties))))
     finally (return :full)))

(defun empty? (board x y)
  (eq (aref board y x) '_))

;;;;;;;;;; General utility
(defun range (min max)
  (loop for i from min to max collect i))

(defun pick (list)
  (nth (random (length list)) list))