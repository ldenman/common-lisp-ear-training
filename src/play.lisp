(in-package :ld-music)

(defun play-random (scale) (note-play (car (random-note scale))))

(defun chord-sequence-play (chord-sequence &optional (sleep 1))
  (dolist (chord (chord-sequence-chords chord-sequence))
    (chord-play chord sleep)))

(defun chord-play (chord &optional (sleep 1))
  (dolist (note (chord-notes chord))
    (when note
      (note-play note)))
  (sleep sleep))

(defun play-chords (chords)
  (dolist (chord chords)
    (chord-play chord)
    (sleep 1)))

(defun play-list-of-notes (list-of-notes)
  (dolist (note list-of-notes)
    (when note
      (schedule-note note))))

(defmacro c (fn &body body) `(,fn (list ,@(mapcar (lambda (x) `',x) body))))

(defun play-tonic (scale) (note-play (car scale)))
(defun play-subdominant (scale) (note-play (nth 3 scale)))
(defun play-dominant (scale) (note-play (nth 4 scale)))

(defun tonic-subdominant-dominant2 (scale)
  (->  (make-scale-chords scale)
       (scale-chord-filter #'chord-filter (lambda (x) (chord-drop-root x scale)))
       (scale-chord-filter #'chord-type-filter #'triads)
       (chord-seq '(I IV V I) 2)))

(defun tonic-subdominant-dominant (scale)
  (progn
    (solfege->notes  scale '(DO MI SO))
    (solfege->notes  scale '(FA LA DO))
    (solfege->notes  scale '(SO TI RE))))

(defun play-tonic-subdominant-dominant (scale)
  (progn
;    (note-play (note-octave-down (attr 'notes (car scale)) (attr 'notes scale)))
    (solfege->notes  scale '(DO MI SO) 4)
    (solfege->notes scale  '(FA LA DO) 4)
    (solfege->notes  scale '(SO TI RE) 4)
;    (play-tonic scale)
    ))

(play-tonic-subdominant-dominant (make-scale 'c4))

(defun smoke-test ()
  (note-play (make-note 'C4 72 nil)))

(defun play-tonic (scale) (note-play (car scale)))
(defun play-subdominant (scale) (note-play (nth 3 scale)))
(defun play-dominant (scale) (note-play (nth 4 scale)))

(defun play-tonic-subdominant-dominant (scale)
  (progn
;    (note-play (note-octave-down (car scale)))
    (solfege->notes scale  '(DO MI SO))
    (solfege->notes  scale '(FA LA DO))
    (solfege->notes scale  '(SO TI RE))

    (play-tonic scale)))

(defun play-tonic-subdominant-dominant3 (scale)
  (chord-sequence-play
   (chord-sequence '(I IV V I) (triads (chord-builder (scale-notes scale))) scale 4 ) 0.5))

