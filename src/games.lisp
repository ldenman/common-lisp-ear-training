(in-package :ld-music)

(defun play-game ()
  (play-tonic-subdominant-dominant *current-scale*)
  (sleep 1)
  (play-random *current-scale*))

(defun set-bass-scale ()
  (let* ((letters '(A B C D E F G))
	 (random-letter (nth (random (length letters)) letters)))
    (set-scale (make-scale-from-pattern
		(intern (format nil "~A~d" random-letter 1))
		(intern (format nil "~A~d" random-letter 2))
		(major-scale)))))

(defun read-guess () (mapcar #'intern (cl-ppcre:split "\\s+" (read-line))))

(setf *playing* t)
(defvar *game-state* '())

;; CODE

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

(defun prompt-bass-guess (answer game)
  (if *playing*
      (progn
	(play-tonic *current-scale*)
	(sleep 0.5)
	(play-subdominant *current-scale*)
	(sleep 0.5)
	(play-dominant *current-scale*)
	(sleep 0.5)
	(play-tonic *current-scale*)
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


(defun run-melody-game (game)
  (loop while *playing* do
    (set-random-scale)
    (sleep 1)
    (prompt-guess (random-notes 3) game)))

(defun run-bass-game (game)
  (loop while *playing* do
    (set-bass-scale)
    (sleep 1)
    (prompt-bass-guess (random-notes 3) game)))


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
