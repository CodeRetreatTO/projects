(defvar *roman-to-arabic* 
  '((?I . 1) (?V . 5) (?X . 10) (?L . 50) (?C . 100) (?D . 500) (?M . 1000)))

(defun lookup-roman (char)
  (cdr (assoc char *roman-to-arabic*)))

(defun parse-aromatic (arom-string)
  (loop for (a r) on (coerce arom-string 'list) by #'cddr
	collect (list (- a 48) (lookup-roman r))))

(defun eval-aromatic (parsed-aromatic)
  (loop for ((a r) (a2 r2)) on parsed-aromatic
	if (and a2 (> r2 r)) collect (- (* a r))
	else collect (* a r)))

(defun aromatic (arom-string)
  (apply #'+ (eval-aromatic (parse-aromatic arom-string))))