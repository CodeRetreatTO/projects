(defun nearest-neighbor (scale input)
  (flet ((stretch (thing) (make-list scale :initial-element thing)))
    (mapcan
     (lambda (ln)
       (stretch (mapcan (lambda (c) (stretch c)) ln)))
     input)))
