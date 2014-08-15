(ql:quickload (list :cl-ppcre :cl-actors))
(defpackage :cryptoarithmetic (:use :cl :cl-ppcre :cl-actors))
(in-package :cryptoarithmetic)

(defun digits->number (digits)
  (loop for d in (reverse digits) for i from 0
     sum (* d (expt 10 i))))

(defun number->digits (num &optional (pad-to 6))
  (loop with tmp = num and digits = nil
     do (multiple-value-bind (rest d) (floor tmp 10)
	  (setf tmp rest)
	  (push d digits))
     sum 1 into len
     until (= pad-to len)
     finally (return digits)))

(defun unique-count (a-string)
  (length (remove #\ (remove-duplicates (coerce a-string 'list)))))

(defun intern-chars (characters &optional (package (sb-int:sane-package)))
  (mapcar (lambda (s) 
	    (if package
		(intern (format nil "~a" s) package))) characters))

(defun intern-word (word)
  (intern-chars (coerce word 'list)))

(defmacro solution-fn (a-string)
  (let* ((s (string-downcase a-string))
	 (words (split " " s))
	 (letters (remove #\  (remove-duplicates (coerce s 'list))))
	 (symbols (intern-chars letters))
	 (keywords (intern-chars (mapcar #'char-upcase letters) :keyword))
	 (non-zeros (intern-chars (remove-duplicates (mapcar (lambda (a) (aref a 0)) words)))))
    `(lambda ,symbols
       (when (and ,@(loop for s in non-zeros collect `(< 0 ,s)) (/= ,@symbols)
		  (= (+ ,@(loop for w in (butlast words)
			     collect `(digits->number (list ,@(intern-word w)))))
		   (digits->number (list ,@(intern-word (car (last words)))))))
	 (list ,@(loop for k in keywords for s in symbols
		    collect k collect s))))))

(defun check (fn arity min max)
  (loop for n from min to max
     when (apply fn (number->digits n arity))
     collect it))

(defmacro solve-for (a-string)
  (let ((len (unique-count a-string)))
    `(check (solution-fn ,a-string) ,len 0 ,(- (expt 10 len) 1))))

;; (solve-for "IT IS OK")
;; (solve-for "TWO TWO FOUR") 
;; (solve-for "THIS MAY JUST WORK") 
;; (solve-for "COLORLESS GREEN IDEAS SLEEP FURIOUSLY")