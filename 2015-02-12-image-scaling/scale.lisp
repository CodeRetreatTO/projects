(defun nearest-neighbor (scale input)
  (mapcan 
   (lambda (ln) 
     (make-list scale :initial-element 
		(mapcan 
		 (lambda (c)
		   (make-list scale :initial-element c))
		 ln)))
	  input))
