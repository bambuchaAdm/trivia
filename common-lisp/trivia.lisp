
(defclass game ()
  ((players
    :accessor players
    :initform ())
   (places
    :accessor places
    :initform (make-array 6))
   (purses
    :accessor purses
    :initform (make-array 6))
   (in-penalty-box 
    :accessor in-penalty-box
    :initform (make-array 6))
   (pop-questions
    :accessor pop-questions
    :initform (loop for i from 1 to 50 collect 
		   (concatenate 'string
				"Pop question "
				(write-to-string i))))
   (science-questions
    :accessor science-questions
    :initform (loop for i from 1 to 50 collect 
		   (concatenate 'string
				"Science question "
				(write-to-string i))))
   (sport-questions
    :accessor sport-questions
    :initform (loop for i from 1 to 50 collect 
		   (concatenate 'string
				"Sport Question "
				(write-to-string i))))
   (rock-questions
    :accessor rock-questions
    :initform (loop for i from 1 to 50 collect 
		   (create-rock-question i)))
   (current-player
    :accessor current-player
    :initform 0)
   (is-getting-out-of-penalty-box
    :accessor is-getting-out-of-penalty-box
    :initform t)))

(defmethod create-rock-question ((index integer))
  (concatenate 'string "Rock Question " (write-to-string index)))

(defmethod how-many-players ((g game))
  (length (players g)))

(defmethod is_playable ((g game))
  (<= 2 (how-many-players g)))

(defgeneric add (game name))

(defmethod add ((game game) (name string))
  (let ((players (players game)))
	(setq players (append players (list name))))
  (let ((index (how-many-players game)))
    (let ((place (elt (places g) index))
	  (purse (elt (purses g) index))
	  (in-penalty-box (elt (in-penalty-box g) index)))
      (setq index 0)
      (setq place 0)
      (setq purse 0)
      (setq in-penalty-box 0))))

(defgeneric roll (game roll))
(defgeneric ask-question (game))
(defgeneric current-category (game))

(defmethod ask-question ((g game))
  )

(defmethod current-category ((g game))
  )


(defmethod roll ((game game)(roll integer))
  (format t "~a is the current player~%" (elt (players g) (current-player g)))
  (format t "They have rolled a ~d~%" roll)

  (if (elt (in-penalty-box g) (current-player g))
      (if (not (eq (mod roll 2) 0))
	  (progn 

	    (setf (is-getting-out-of-penalty-box g) T)
	    (format t "~a is getting out of the penalty box~%" (elt (players g) (current-player g)))
	    (incf (elt (places g) (current-player g)) roll)
	    (when (> (elt (places g) (current-player g)) 11)
	      (decf (elt (places g) (current-player g)) 12))
	    (format t "~a's new location is ~a~%"
		    (elt (players g) (current-player g))
		    (elt (places g) (current-player g)))
	    (format t "The category is ~a~%" (current-category g))
	    (ask-question g))
	  (progn 
	    (format t "~a is not getting out of the penalty box~%" 
		    (elt (players g) (current-player g)))
	    (setf (is-getting-out-of-penalty-box g) nil)))
      (progn
	(incf (elt (places g) (current-player g)) roll)
	(when (> (elt (places g) (current-player g)) 11)
	  (decf (elt (places g) (current-player g)) 12))
	(format t "~a's new location is ~a~%"
		(elt (players g) (current-player g))
		(elt (places g) (current-player g)))
	(format t "The category is ~a~%" (current-category g))
	(ask-question g))))
