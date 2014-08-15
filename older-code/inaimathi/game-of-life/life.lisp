(defpackage :life (:use :cl))
(in-package :life)

(defun moore-neighborhood (cell)
  (let ((r '(-1 0 1)))
    (mapcan
	 (lambda (delta-x)
	   (loop for delta-y in r
	      unless (and (= delta-x 0) (= delta-y 0))
	      collect (cons (+ (car cell) delta-x) (+ (cdr cell) delta-y))))
	 r)))

(defun compute-frequencies (cells)
  (let ((lonely (make-hash-table :test #'equal)) 
	(h (make-hash-table :test #'equal)))
    (loop for a-cell being the hash-keys of cells
       do (loop for c in (moore-neighborhood a-cell)
	     do (let ((res (incf (gethash c lonely 0))))
		  (cond 
		    ((or (= res 2) (= res 3)) (setf (gethash c h) res))
		    ((= res 4) (remhash c h))))))
    h))

(defun cells->hash (cells)
  (let ((h (make-hash-table :test #'equal :size 800)))
    (loop for c in cells
	 do (setf (gethash c h) 0))
    h))

(defun life-step (cells world-size)
  (let ((f (compute-frequencies cells)))
    (loop for k being the hash-keys in f
       for (x . y) = k
       when (or 
	     (and (= (gethash k f) 2) (not (gethash k cells))))
       do (remhash k f))
    f))

(defun print-world (live-cells world-size)
  (dotimes (y world-size)
    (dotimes (x world-size)
      (if (gethash (cons x y) live-cells)
	  (format t "X")
	  (format t ".")))
    (format t "~%")))

(defun run-life (world-size steps cells)
  (let ((world (cells->hash cells)))
    (loop 
       repeat steps
       do (setf world (life-step world world-size)))
    world))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;; data related
(defun .cells->list (filename)
  (with-open-file (stream filename)
    (apply #'append
	   (loop with y = 0
	      for line = (read-line stream nil 'eof) until (eq line 'eof)
	      unless (char= (aref line 0) #\!)
	      collect (let ((line (loop for x from 0
				     for char being the elements of line
				     when (char= char #\O) collect (cons x y)))) 
			(incf y)
			line)))))

(defparameter *blinker* '((1 . 2) (2 . 2) (3 . 2)))
(defparameter *glider* '((1 . 0) (2 . 1) (0 . 2) (1 . 2) (2 . 2)))
(defparameter *gosper-glider-gun* 
  '((24 . 0) (22 . 1) (24 . 1) (12 . 2) (13 . 2) (20 . 2) (21 . 2) (34 . 2)
    (35 . 2) (11 . 3) (15 . 3) (20 . 3) (21 . 3) (34 . 3) (35 . 3) (0 . 4) (1 . 4)
    (10 . 4) (16 . 4) (20 . 4) (21 . 4) (0 . 5) (1 . 5) (10 . 5) (14 . 5) (16 . 5)
    (17 . 5) (22 . 5) (24 . 5) (10 . 6) (16 . 6) (24 . 6) (11 . 7) (15 . 7)
    (12 . 8) (13 . 8)))