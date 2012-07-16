(defun create-rock-question (index)
  (concatenate 'string "Rock Question " (write-to-string index)))

(defclass game ()
  ((players
    :accessor game-players
    :initform ())
   (places
    :accessor game-places
    :initform (make-array 6))
   (purses
    :accessor game-purses
    :initform (make-array 6))
   (in-penalty-box 
    :accessor game-in-penalty-box
    :initform (make-array 6))
   (pop-questions
    :accessor game-pop-questions
    :initform (loop for i from 1 to 50 collect 
		   (concatenate 'string
				"Pop question "
				(write-to-string i))))
   (science-questions
    :accessor game-science-questions
    :initform (loop for i from 1 to 50 collect 
		   (concatenate 'string
				"Science question "
				(write-to-string i))))
   (sport-questions
    :accessor game-sport-questions
    :initform (loop for i from 1 to 50 collect 
		   (concatenate 'string
				"Sport Question "
				(write-to-string i))))
   (rock-questions
    :accessor game-rock-questions
    :initform (loop for i from 1 to 50 collect 
		   (create-rock-question i)))
   (current-player
    ;:accesor game-current-player
    :initform 0)
   (is-getting-out-of-penalty-box
    :initform nil)))
