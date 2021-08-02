(in-package :ld-music)

(defun play-game ()
  (play-tonic-subdominant-dominant *current-scale*)
  (sleep 1)
  (play-random *current-scale*))

(defun set-bass-scale ()
  (let* ((letters '(A B C D E F G))
	 (random-letter (nth (random (length letters)) letters)))
    (make-scale-from-template
		(intern (format nil "~A~d" random-letter 1))
		(intern (format nil "~A~d" random-letter 2))
		(major-scale-template))))

(defun read-guess () (mapcar #'intern (cl-ppcre:split "\\s+" (read-line))))

(setf *playing* t)
(defvar *game-state* '())

;; CODE

(defun solfege-trainer ()
  (let* ((scale (make-scale 'c4))
	 (notes (scale-range 'c2 'c3 (attr 'scale scale))))

    (dolist (l (loop
		 for x from 0 to 100
		 collect (random-element notes) )
	       )
      (princ (note-solfege l))
      (finish-output)
      (note-play l)
      (sleep 1)
      )

    ))


;(setf *playing* t)
;(solfege-trainer)

(defun make-game (name logic-fn)
  (acons 'correct nil
	 (list (list 'name name)
	       (list 'answers nil)
	       (list 'playing nil)
	       (list 'game logic-fn))))
(defun update-game-lst (key item game)
  (if item
      (let ((newlist (append (cdr (assoc key game)) (list item))))
	(setf (cdr (assoc key game)) newlist))))

(defun my-play-game (game)
  (setf *playing* t)
  (funcall (car (cdr (assoc 'game game))) game))

(defvar *my-game* nil)
(defun play ()
  (setf *my-game*  (make-game 'melody #'run-melody-game))
  (pm-reload)
  *my-game*
  (my-play-game *my-game*))
(defun chord-trainer ()
  (setf *my-game*  (make-game 'chord-trainer #'run-chord-trainer))
  (pm-reload)
  *my-game*
  (my-play-game *my-game*)
  )

(defun play-bass-game ()
  (setf *bass-game*  (make-game 'bass #'run-bass-game))
  (pm-reload)
  (my-play-game *bass-game*))

;(play-bass-game)
;(play-tonic *current-scale*)
;(pm-reload)
;(play)

(defun find-answers (type game)
  (remove-if-not (lambda (item) (eq type (cdr (car item))))
		   (cdr (assoc 'answers game))))

;; NEED TO FIX MODES FN
;; (defun make-chords (scale)
;;   (mapcar (lambda (s) (loop for (x) on s by #'cddr collect x)  )
;; 	  (modes scale)))
;; (defun subseq-chord (p1 p2 lofchords)
;;   (mapcar (lambda (n) (subseq n p1 p2))
;; 	  (make-chords lofchords)))
;; (defun triad-chords (chords)
;;   (subseq-chord 0 3 chords))
;; (defun seventh-chords (chords)
;;   (subseq-chord 0 4 chords))

(defmacro dbug (code)
  `(progn (princ (format nil "~A" ,code))
	  ,code))

(defun find-unique-answers (type game)
  (remove-duplicates (mapcar (lambda (i) (cons (car i) (assoc 'scale i))) (find-answers type game)) :test #'equal))

(defun repeat-answers (type game)
  (setf *newgame* (make-game 'new-game (lambda ())))
  (let ((answers (remove-duplicates (find-answers type game) :test (lambda (x y) (equal (car x) (car y))))))
    (setf *playing* t)
    (dolist (a answers)
      (let ((scale (car (cdr (assoc 'scale (cdr a))))))
	(setf *current-scale* scale)
	(prompt-guess (car (car a)) *newgame*)))))
;(repeat-answers 'incorrect *bass-game*)
(defun update-score (score game)
  (setf (cdr (assoc 'score game)) score)
  game)

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

(defun scale-chord-filter (fn &rest args)
  (lambda (chord-data)
    (let ((chords (funcall (apply fn args) (attr 'chords chord-data))))
      (attr= chords 'chords chord-data)
      (attr= (chord-roman-numerals chords) 'roman-numeral-chords chord-data)
      chord-data)))

(defun rebuild-chords ()
  (lambda (chord-data)
    (attr 'chords (make-scale-chords (attr 'scale chord-data)))))

(defun octave-filter (octave)
  (lambda (chords)
    (remove-if-not
     (lambda (chord)
       (= octave (note-attr (chord-tone-note (car chord)) 'octave)))
     chords)))

(defun chord-filter (fn)
  (lambda (chord-data) (mapcar fn chord-data)))

(defun chord-type-filter (fn)
  (lambda (chord-data) (funcall fn chord-data)))

(defun chord-seq (seq &optional (octave 4))
  (lambda (chord-data)
    (chord-sequence seq (attr 'chords chord-data) octave)))

(defun chord-seq-play (chord-seq)
  (chord-sequence-play (chord-sequence-chords chord-seq)))

;; (-> (make-scale-chords (make-scale 'C2))
;;     (scale-chord-filter #'chord-type-filter #'sevenths)
;;     (scale-chord-filter #'chord-filter #'chord-butfifth)
;;     (chord-seq '(II-
;; 		 (octave . 2)
;; 		 V
;; 		 (octave . 3)
;; 		 I
;; 		 (octave . 3)
;; 		 VI-
;; 		 (octave . 3)
;; 		 II-
;; 		 (octave . 2)
;; 		 V
;; 		 (octave . 3)
;; 		 I
;; 		 I
;; 		 ) 3)

;;       #'chord-seq-play)

;(pm-reload)
;(quick-test)

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
