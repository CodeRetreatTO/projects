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
  (sort
   (loop for ln in (apply #'mapcar #'list e-map)
      for i from 0
      collect (make-instance 'seam :weight (apply #'+ ln) :indices (list i)))
   #'< :key #'weight))

(defmethod remove-single-seam! ((lines list) (seam number))
  (mapc 
   (lambda (ln)
     (setf (char ln seam) #\space))
   lines)
  lines)

(defmethod remove-seams! ((lines list) (seams list))
  (mapc (lambda (s) (remove-single-seam! lines s)) seams)
  lines)
