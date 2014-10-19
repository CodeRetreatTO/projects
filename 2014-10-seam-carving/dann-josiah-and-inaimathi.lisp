;;;;; Seam-related stuff
(defclass seam ()
  ((weight :accessor weight :initform 0 :initarg :weight)
   (indices :accessor indices :initform nil :initarg :indices)
   (index-count :accessor index-count :initform 0 :initarg :index-count)
   (last-index :accessor last-index :initform nil :initarg :last-index)))

(defun make-seam (&optional initial-index/weight-pairs)
  (multiple-value-bind (ixs w ct) (loop for (ix w) on initial-index/weight-pairs by #'cddr
				     collect ix into indices sum w into weight sum 1 into index-ct
				     finally (return (values indices weight index-ct)))
    (make-instance 'seam :indices ixs :index-count ct :weight w :last-index (last ixs))))

(defmethod copy ((s seam))
  (let ((ixs (copy-list (indices s))))
    (make-instance 'seam :indices ixs :index-count (index-count s)
		   :weight (weight s) :last-index (last ixs))))

(defmethod add-cell! ((seam seam) (index cons) (weight number))
  (let ((s (copy seam)))
    (setf (cdr (last-index s)) (list index)
	  (last-index s) (cdr (last-index s)))
    (incf (weight s) weight)
    (incf (index-count s))
    s))

;;;;; Basic utility
(defmethod take ((count number) (lst list))
  (loop for elem in lst repeat count collect elem))

;;;;; IO Routines
(defmethod get-lines ((fname string))
  (with-open-file (s fname)
    (loop for ln = (read-line s nil nil)
       while ln collect ln)))

(defmethod put-lines ((fname string) (lines list))
  (with-open-file (s fname :direction :output :if-does-not-exist :create :if-exists :supersede)
    (mapc (lambda (ln) (write-line ln s)) lines)))

;;;;; Actually finding seams
(defmethod energy-map ((lines list))
  (mapcar 
   (lambda (ln)
     (loop for (a b) on (coerce ln 'list) while b
	collect (abs (- (char-code a) (char-code b)))))
   lines))

(defmethod seams-in ((e-map list))
  (let ((seams (loop for w in (first e-map) for i from 0
		  collect (make-seam `((,i 0) ,w)))))
    (loop for ln in (rest e-map) for ln-ix from 1
       do (setf seams
		(loop for w in ln for w-ix from 0
		   append (let* ((seam 
				  (first 
				   (sort (remove-if-not
					  (lambda (s) (<= (- w-ix 1) (caar (last-index s)) (+ w-ix 1)))
					  seams)
					 #'< :key #'weight))))
			    (when seam (list (add-cell! seam `(,w-ix ,ln-ix) w)))))))
    (remove-duplicates 
      (sort seams #'< :key #'weight)
      :from-end t
      :key (lambda (s) (caar (indices s))))))

(defmethod mask-seam! ((lines list) (s seam) &optional (mask #\space))
  (loop for ln in lines for ix in (indices s)
     do (setf (char ln (first ix)) mask))
  lines)

(defmethod remove-seam! ((lines list) (s seam))
  (mapcar (lambda (ln) (remove #\space ln))
	  (mask-seam! lines s #\space)))

(defmethod scale-by! ((lines list) (count number))
  (loop with lns = lines repeat count
     do (setf lns (remove-seam! lns (first (seams-in (energy-map lns)))))
     finally (return lns)))
