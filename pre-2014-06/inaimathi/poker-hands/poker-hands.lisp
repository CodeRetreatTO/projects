;; poker-hands.lisp

(defpackage :poker (:use :cl :split-sequence))
(in-package :poker)

(defparameter *letter->val* '(#\T 10 #\J 11 #\Q 12 #\K 13 #\A 14))
(defparameter *hand-type->val* '(:high-card 1 :pair 2 :two-pairs 3 :three-of-a-kind 4 
                                 :straight 5 :flush 6 :full-house 7 
                                 :four-of-a-kind 8 :straight-flush 9))

(defclass card ()
  ((rank :reader rank :initarg :rank)
   (suit :reader suit :initarg :suit)))

(defun read-card (card-string)
  (make-instance 'card
                 :rank (or (getf *letter->val* (aref card-string 0))
                           (parse-integer card-string :junk-allowed t))
                 :suit (aref card-string 1)))

(defun read-hand (hand-string)
  (sort (mapcar #'read-card (split-sequence #\space hand-string))
        #'> :key #'rank))

(defun flush-p (cards) 
  (let ((suits (mapcar #'suit cards)))
    (every (lambda (s) (eq s (car suits))) (cdr suits))))

(defun range (start end)
  (loop for i from start to end collect i))

(defun straight-p (cards)
  (equal (mapcar #'rank cards)
         (loop repeat (length cards) 
            for i from (rank (car cards)) downto 0
            collect i)))

(defun find-sets (cards)
  (let ((copy (copy-list cards)))
    (loop for c in copy
       when (remove (rank c) cards :key #'rank :test-not #'=) collect it
       do (setf cards (delete (rank c) cards :key #'rank)))))

(defun set-of-p (n sets)
  (some (lambda (s) (= (length s) n)) sets))

(defun count-sets-of (n sets)
  (count-if (lambda (s) (= (length s) n)) sets))

(defun hand-type (hand)
  (let ((sets (find-sets hand)))
    (cond ((and (flush-p hand) (straight-p hand)) :straight-flush)
          ((set-of-p 4 sets) :four-of-a-kind)
          ((and (set-of-p 3 sets) (set-of-p 2 sets)) :full-house)
          ((flush-p hand) :flush)
          ((straight-p hand) :straight)
          ((set-of-p 3 sets) :three-of-a-kind)
          ((= 2 (count-sets-of 2 sets)) :two-pairs)
          ((set-of-p 2 sets) :pair)
          (t :high-card)))) 

(defmethod break-tie (hand-type (hand-a list) (hand-b list))
  (loop for a in hand-a
        for b in hand-b
        unless (= (rank a) (rank b))
          do (return (> a b))))

(defun hand-type-> (hand-type-a hand-type-b)
  (> (getf *hand-type->val* hand-type-a)
     (getf *hand-type->val* hand-type-b)))

(defun hand-> (hand-a hand-b)
  (let ((type-a (hand-type hand-a))
        (type-b (hand-type hand-b)))
    (or (hand-type-> type-a type-b)
        (when (eq type-a type-b)
        (break-tie type-a hand-a hand-b)))))
