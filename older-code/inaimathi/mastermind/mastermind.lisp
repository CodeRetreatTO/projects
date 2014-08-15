(defun take (count lst)
  (loop for elem in lst repeat count collect elem))

(defun range (min max)
  (loop for i from min to max collect i))

(defmethod pick ((seq list))
  (nth (random (length seq)) seq))

(defun map-remove (item lists)
  (mapcar (lambda (lst) (remove item lst)) lists))

(defun rnd (in)
  (declare (ignore in))
  (random 40))

(defun make-checker ()
  (let ((secret (take 10 (sort (range 1 40) #'>  :key #'rnd))))
    (format t "~s~%" secret)
    (lambda (guess)
      (mapcar (lambda (g s)
		(cond ((= g s) :ok)
		      ((member g secret) :in)
		      (t :out)))
	      guess
	      secret))))

(defparameter oracle (make-checker))

;;; Still wasteful. It keeps known numbers in instead of using the space for more accurate guesses.
;;; Keep a tally of all numbers between 1 and 40 inclusive. Remove the ones that are either :out or :ok, 
(defun solve (oracle)
  (loop with possibilities = (make-list 10 :initial-element (range 1 40))
     for guess-count from 1
     do (if (every (lambda (p) (not (cdr p))) possibilities)
	    (return (mapcar #'car possibilities))
	    (let ((guess (loop for p in possibilities collect (pick p))))
	      (format t "Guess #~a :: ~s~%" guess-count guess)
	      (loop for i from 0 for g in guess for r in (funcall oracle guess)
		 do (case r
		      (:ok (setf possibilities (map-remove g possibilities)
				 (nth i possibilities) (list g)))
		      (:in (setf (nth i possibilities) (remove g (nth i possibilities))))
		      (:out (setf possibilities (map-remove g possibilities)))))))))