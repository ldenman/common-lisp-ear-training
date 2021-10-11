(in-package :ld-music)

;; Game constructor functions
(defun make-game (name logic-fn)
  (acons 'correct nil
	 (list
	 (cons 'score nil)
	 (cons 'state nil)
	 (cons 'name name)
	 (cons 'answers nil)
	 (cons 'playing nil)
	 (cons 'game logic-fn))))

;; Generic Game functions ;;
(defun update-game-lst (key item game)
  (if item
      (let ((newlist (append (cdr (assoc key game)) (list item))))
	(attr= newlist key game))))

(defun my-play-game (game)
  (attr= t 'playing game)
  (funcall (attr 'game game) game))

(defun find-answers (type game)
  (remove-if-not (lambda (item) (eq type (cdr (car item))))
		   (attr 'answers game)))

(defun find-unique-answers (type game)
  (remove-duplicates (mapcar (lambda (i)
			       (cons (car i) (attr 'scale i))) (find-answers type game)) :test #'equal))

(defun playing-p (game)
  (equal 't (attr 'playing game)))

;;unused
;; (defun repeat-answers (type game)
;; ;  (setf *newgame* (make-game 'new-game (lambda ())))
;;   (let* ((answers (remove-duplicates (find-answers type game) :test (lambda (x y) (equal (car x) (car y))))))
;;     (attr= 't 'playing game)
;;     (dolist (a answers)
;;       (let ((scale (attr 'scale (cdr a))))
;; 	(setf *current-scale* scale)
;; 	(prompt-guess (car (car a)) game)))))

;; (defun score (game)
;;   (let ((correct-count (loop for i in (attr 'state game) count (equalp i 1)))
;; 	(x (length (attr 'state game))))
;;     (attr= (round (* 100.0 (/ correct-count x))) 'score game)))

;;unused
;; (defun stop-game (game)
;;   (setf *playing* nil)
;;   game)

(defun read-guess () (mapcar #'intern (cl-ppcre:split "\\s+" (read-line))))
;; END GENERIC GAME FUNCTIONS ;;

;;;; Games
;; BASS GAMES

;; END BASS GAME


;; WIP: SOLFEGE TRAINER GAME
;; (defun solfege-trainer ()
;;   (let* ((scale (make-scale 'c4))
;; 	 (notes (note-range 'c2 'c3 (scale-notes scale))))

;;     (dolist (l (loop
;; 		 for x from 0 to 100
;; 		 collect (random-element notes) )
;; 	       )
;;       (princ (note-solfege l))
;;       (finish-output)
;;       (note-play l)
;;       (sleep 1))))
;(solfege-trainer)
;; END SOLFEGE TRAINER GAME

;; MELODY GAME ;;
(defun run-melody-game (game)
  (loop while (playing-p game) do
    (let ((scale  (random-scale2 (major-scale-template))))
      (sleep 1)
      (prompt-guess (random-notes 3 (scale-octave-range 3 5 (scale-notes scale))) game scale))))

(defun play-melody-game ()
  (my-play-game (make-game 'melody #'run-melody-game)))

(defun prompt-guess (answer game current-scale)
  (if (playing-p game)
      (progn
	(play-tonic-subdominant-dominant current-scale)
	(sleep 1)
	(dolist (a answer)
	  (note-play a)
	  (sleep 1))
	(let ((guess (read-guess)))
	  (if (string= "stop" (symbol-name (car guess)))
	      (attr= 'f 'playing game)
	      (if (equal (mapcar #'string-upcase (mapcar #'symbol-name guess))
			 (mapcar #'symbol-name (mapcar #'note-solfege answer)))
		  (progn
		    (update-game-lst 'answers (acons answer 'correct
						     (list
						      (list 'scale current-scale))) game)
		    (write-line "good")
		    )
		  (progn
		    (update-game-lst 'answers (acons answer 'incorrect
						     (list
						      (list 'guess guess)
						      (list 'scale current-scale))) game)

;		    (setf *game-state* (append *game-state* (list 0))) ;; USE LOCAL GAME VAR

		    (prompt-guess answer game current-scale))))))))

;; (play-melody-game)
;; END MELODY GAME ;;

;; CHORD TRAINER GAME ;;
(defun run-chord-trainer (game)
  (loop while (playing-p game) do
    (sleep 1)
    (let* ((scale (random-scale2 (major-scale-template) '(1 . 4)))
	   (chords (attr 'roman-numeral-chords (make-scale-chords scale)))
	   (random-chord (nth (random (length chords)) chords)))
      (prompt-chord-guess random-chord game scale))))

(defun play-chord-trainer ()
  (let ((game (make-game 'chord-trainer #'run-chord-trainer)))
    (my-play-game game)))

(defun prompt-chord-guess (answer game scale)
  (if (playing-p game)
      (progn
	(play-tonic-subdominant-dominant scale 3)
        (sleep 1)
					;        (note-play (note-octave-down (car (cdr answer)) (scale-notes scale)))
	(chord-play (cdr answer))
	(let ((guess (read-guess)))
	  (if (string= "stop" (symbol-name (car guess)))
	      (attr= nil 'playing game)
	      (if (equal (car (mapcar #'string-upcase (mapcar #'symbol-name guess)))
			 (symbol-name (car answer)))
		  (progn
		    (dolist (note (chord-notes (cdr answer)))
		      (note-play note)
		      (sleep 0.5))
		    (dolist (note (rest (reverse (chord-notes (cdr answer)))))
		      (note-play note)
		      (sleep 0.5))
		    (chord-play (cdr answer))		    
		    (update-game-lst 'answers (acons answer 'correct
						     (list
						      (list 'scale scale))) game)
		    (write-line "good"))
		  (progn
		    (update-game-lst 'answers (acons answer 'incorrect
						     (list
						      (list 'guess guess)
						      (list 'scale scale))) game)
;		    (setf *game-state* (append *game-state* (list 0))) ;; USE LOCAL GAME VAR

		    (prompt-chord-guess answer game scale))))))))
;; (play-chord-trainer)
;; END CHORD TRAINER GAME ;;

;; BASS TRAINER GAME ;;
(defun set-bass-scale ()
  (let* ((letters '(A  ))
	 (random-letter (nth (random (length letters)) letters)))
    (make-scale-from-template
		(intern (format nil "~A~d" random-letter 1))
		(intern (format nil "~A~d" random-letter 2))
		(major-scale-template))))

(defun play-bass-game ()
  (my-play-game (make-game 'bass #'run-bass-game)))

(defun run-bass-game (game)
  (loop while (playing-p game) do
    (sleep 1)
    (with-scale (set-bass-scale)
      (prompt-bass-guess (random-notes 3 *current-scale*) game *current-scale*))))

(defun prompt-bass-guess (answer game scale)
  (if (playing-p game)
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
	      (attr= 'f 'playing game)
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

;		    (setf *game-state* (append *game-state* (list 0))) ; USE LOCAL GAME VAR
		    (prompt-bass-guess answer game scale))))))))
; (play-bass-game)
;; END BASS TRAINER GAME ;;


