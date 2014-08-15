(defpackage :tic (:use :cl))
(in-package :tic)

(defvar *empty*
  '((_ _ _)
    (_ _ _)
    (_ _ _)))

(defmethod other (player)
  (if (eq player '0)
      'X
      '0))

(defmethod pick ((thing list))
  (nth (random (length thing)) thing))

(defun nth-empty (n)
  (let ((ct 0))
    (lambda (elem)
      (when (eq elem '_)
	(let ((found? (= ct n)))
	  (incf ct)
	  found?)))))

(defun all-moves (board &optional (you 'O))
  (loop for i from 0 
     for sub = (subst-if you (nth-empty i) board)
     until (equal sub board)
     collect sub))

(defun non-losing-future? (board &optional (you '0))
  (mapcar (lambda (b) (all-moves b (other you)))
	  (all-moves board you)))

(defun player-won? (board player)
  (labels ((player? (sym) (eq sym player))
	   (any-filled? (lines)
	     (some 
	      (lambda (line) (every #'player? line))
	      lines)))
    (or (any-filled? board)
	(any-filled? (apply #'mapcar #'list board))
	(and (player? (cadadr board))
	     (or (every #'player? (list (caar board) (second (cdaddr board))))
		 (every #'player? (list (caddar board) (caaddr board))))))))

(defun complete? (board)
  (or (player-won? 'X) (player-won? 'O)))

(defun make-move (board &optional (you '0))
  (pick (all-moves board you)))
