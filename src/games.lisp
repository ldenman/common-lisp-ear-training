(in-package :ld-music)

;; Game constructor functions
(setf *playing* t)
(defvar *game-state* '())
(defvar *my-game* nil)

(defun make-game (name logic-fn)
  (acons 'correct nil
	 (list (list 'name name)
	       (list 'answers nil)
	       (list 'playing nil)
	       (list 'game logic-fn))))

;; Game functions
(defun play-game ()
  (play-tonic-subdominant-dominant *current-scale*)
  (sleep 1)
  (play-random *current-scale*))

(defun update-game-lst (key item game)
  (if item
      (let ((newlist (append (cdr (assoc key game)) (list item))))
	(setf (cdr (assoc key game)) newlist))))

(defun my-play-game (game)
  (setf *playing* t)
  (funcall (car (cdr (assoc 'game game))) game))

(defun find-answers (type game)
  (remove-if-not (lambda (item) (eq type (cdr (car item))))
		   (cdr (assoc 'answers game))))

(defun find-unique-answers (type game)
  (remove-duplicates (mapcar (lambda (i)
			       (cons (car i) (assoc 'scale i))) (find-answers type game)) :test #'equal))

(defun repeat-answers (type game)
  (setf *newgame* (make-game 'new-game (lambda ())))
  (let ((answers (remove-duplicates (find-answers type game) :test (lambda (x y) (equal (car x) (car y))))))
    (setf *playing* t)
    (dolist (a answers)
      (let ((scale (car (cdr (assoc 'scale (cdr a))))))
	(setf *current-scale* scale)
	(prompt-guess (car (car a)) *newgame*)))))

(defun update-score (score game)
  (setf (cdr (assoc 'score game)) score)
  game)


;; Guess functions
(defun read-guess () (mapcar #'intern (cl-ppcre:split "\\s+" (read-line))))

;; Games
;; Bass Game
(defun set-bass-scale ()
  (let* ((letters '(A B C D E F G))
	 (random-letter (nth (random (length letters)) letters)))
    (make-scale-from-template
		(intern (format nil "~A~d" random-letter 1))
		(intern (format nil "~A~d" random-letter 2))
		(major-scale-template))))

(defun play-bass-game ()
  (setf *bass-game*  (make-game 'bass #'run-bass-game))
  (pm-reload)
  (my-play-game *bass-game*))

;; Solfege Trainer
(defun solfege-trainer ()
  (let* ((scale (make-scale 'c4))
	 (notes (scale-range 'c2 'c3 (scale-notes scale))))

    (dolist (l (loop
		 for x from 0 to 100
		 collect (random-element notes) )
	       )
      (princ (note-solfege l))
      (finish-output)
      (note-play l)
      (sleep 1))))

(defun play-melody-game ()
  (setf *my-game*  (make-game 'melody #'run-melody-game))
  *my-game*
  (my-play-game *my-game*))

(defun play-chord-trainer ()
  (setf *my-game*  (make-game 'chord-trainer #'run-chord-trainer))
  *my-game*
  (my-play-game *my-game*))

(defun prompt-guess (answer game)
  (if *playing*
      (progn
	(play-tonic-subdominant-dominant *current-scale*)
	(sleep 1)
	(dolist (a answer)
	  (note-play (car a))
	  (sleep 1))
	(let ((guess (read-guess)))
	  (if (string= "stop" (symbol-name (car guess)))
	      (setf *playing* nil)
	      (if (equal (mapcar #'string-upcase (mapcar #'symbol-name guess))
			 (mapcar #'symbol-name (mapcar #'cdr answer)))
		  (progn
		    (update-game-lst 'answers (acons answer 'correct
						     (list
						      (list 'scale *current-scale*))) game)
		    (write-line "good")
		    )
		  (progn
		    (update-game-lst 'answers (acons answer 'incorrect
						     (list
						      (list 'guess guess)
						      (list 'scale *current-scale*))) game)
		    (setf *game-state* (append *game-state* (list 0)))
		    (prompt-guess answer game))))))))

(defun prompt-bass-guess (answer game scale)
  (if *playing*
      (progn
	(play-tonic scale)
	(sleep 0.5)
	(play-subdominant scale)
	(sleep 0.5)
	(play-dominant scale)
	(sleep 0.5)
	(play-tonic scale)
	(sleep 1)
	(dolist (a answer)
	  (note-play a)
	  (sleep 1))
	(let ((guess (read-guess)))
	  (if (string= "stop" (symbol-name (car guess)))
	      (setf *playing* nil)
	      (if (equal (mapcar #'string-upcase (mapcar #'symbol-name guess))
			 (mapcar #'symbol-name (mapcar #'note-solfege answer)))
		  (progn
		    (update-game-lst 'answers (acons answer 'correct
						     (list
						      (list 'scale scale))) game)
		    (write-line "good")
		    )
		  (progn
		    (update-game-lst 'answers (acons answer 'incorrect
						     (list
						      (list 'guess guess)
						      (list 'scale scale))) game)
		    (setf *game-state* (append *game-state* (list 0)))
		    (prompt-bass-guess answer game scale))))))))


(defun prompt-chord-guess (answer game scale)
  (if *playing*
      (progn
	(play-tonic-subdominant-dominant scale)
        (sleep 1)
        (note-play (note-octave-down (car (cdr answer))))
	(chord-play (cdr answer))
	(let ((guess (read-guess)))
	  (if (string= "stop" (symbol-name (car guess)))
	      (setf *playing* nil)
	      (if (equal (car (mapcar #'string-upcase (mapcar #'symbol-name guess)))
			 (symbol-name (car answer)))
		  (progn
		    (dolist (note (cdr answer))
		      (note-play note)
		      (sleep 0.5)
		      )
		    (dolist (note (rest (reverse (cdr answer))))
		      (note-play note)
		      (sleep 0.5)
		      )
		    (chord-play (cdr answer))		    
		    (update-game-lst 'answers (acons answer 'correct
						     (list
						      (list 'scale scale))) game)
		    (write-line "good")
		    )
		  (progn
		    (update-game-lst 'answers (acons answer 'incorrect
						     (list
						      (list 'guess guess)
						      (list 'scale scale))) game)
		    (setf *game-state* (append *game-state* (list 0)))
		    (prompt-chord-guess answer game scale))))))))
;(chord-trainer)

(defun run-chord-trainer (game)
  (loop while *playing* do
    (sleep 1)
    (let* ((chords (remove-if-not (lambda (x) (= 4 (note-attr (car (cdr x )) 'octave))) (make-chords 'C4 #'triads)))
	   (random-chord (nth (random (length chords)) chords)))
      (prompt-chord-guess random-chord game (scale-range 'C4 'G5 (make-scale 'C4))))))

(defun run-melody-game (game)
  (loop while *playing* do
    (set-random-scale)
    (sleep 1)
    (prompt-guess (random-notes 3) game)))

(defun run-bass-game (game)
  (loop while *playing* do
    (sleep 1)
    (with-scale (set-bass-scale)
      (prompt-bass-guess (random-notes 3 *current-scale*) game *current-scale*))))

;(run-melody-game)

(defun run-game ()
  (loop while *playing* do
    (set-random-scale)
    (sleep 1)
    (let ((answer (random-note *current-scale*)))
      (play-tonic-subdominant-dominant *current-scale*)
      (sleep 1)
      (note-play (car answer))
      (let ((guess (read)))
	(if (eq guess (note-solfege answer))
	    (progn
	      (setf *game-state* (append *game-state* (list 1)))
	      (write-line "good")
	      
	      )
	    (setf *game-state* (append *game-state* (list 0))))))))

(defun score ()
  (if *game-state*
  (let ((correct-count (loop for i in *game-state* count (equalp i 1)))
	(x (length *game-state*)))
	(format nil "~d%" (round (* 100.0 (/ correct-count x)))))))
(defun stop-game (game)
  (setf *playing* nil)
  (setf *my-game* nil)
  game)

(defun reset-game ()
  (pm-reload)
  (setf *playing* nil)
  (setf *playing* t)
  (setf *game-state* '())
  (run-game))
