(in-package :ld-music)

(defun common-chord-progressions ()
  '((I III- IV V I)
    (I II- III- VI- IV V I)
    (I VI- II- V I VII I)
    (I VI- II- V I)
    (I V VI- IV I)))

(defun realize-common-progressions (key)
  (mapcar (lambda (progression)
	    (chord-sequence progression (make-chords key)))
	  (common-chord-progressions)))

(defun play-random-progression (key)
  (dolist (chord (mapcar #'cdr (nth (random (length (common-chord-progressions))) (realize-common-progressions key))))
    (note-play (note-octave-down (car chord)))
    (chord-play chord)
    (sleep 1)))







