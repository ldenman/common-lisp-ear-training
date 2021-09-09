(in-package :ld-music)

(defun attach-type (type contents)
  (cons type contents))
(defun whattype (datum)
  (car datum))
(defun contents (datum)
  (cdr datum))

(defun chord-sequence? (item) (equal (whattype item) 'chord-sequence))
(defun cadence-sequence? (item) (equal (whattype item) 'cadence-sequence))
(defun note? (item) (equal (attr 'type item) 'note))
(defun chord-tone? (item) (equal (attr 'type item) 'chord-tone))
(defun chord? (item) (equal (attr 'type item) 'chord))
(defun scale? (item) (equal (attr 'type item) 'scale))

(defun _notes (list)
  (cond ((scale? list) (attr 'notes list))
	((cadence-sequence?  list ))
	((chord? list) (mapcar #'_note list))))
(defun _note (item)
  (cond ((note? item) item)
	((chord-tone? item) (chord-tone-note item))))

(defun play (item)
  (cond
    ((note? item) (note-play item))
    ((cadence-sequence? item) (dolist (chord (contents item))
				(play-chord-notes-nonasync (mapcar #'_note chord))))
    ((chord-sequence? item) (chord-sequence-play (contents item)))
    ((chord? item) (chord-play-nonasync item))
    ((listp item) (play-chord-notes-nonasync item))))

(defun play-random (scale) (note-play (car (random-note scale))))

(defun chord-play-nonasync (chord &optional (sleep 0.5))
  (play-chord-notes-nonasync (chord-notes chord)))

(defun chord-sequence-play (chord-sequence &optional (sleep 1))
  (dolist (chord (chord-sequence-chords chord-sequence))
    (chord-play-nonasync chord)
    (mysleep sleep)))

(defun chord-play (chord &optional (sleep 1))
  (chord-play-nonasync chord sleep))
;(smoke-test)



(defun play-chord-notes-nonasync (notes &optional (sleep 1))
  (dolist (note notes)
    (when note
      (note-play note)))
  (mysleep sleep))

(defun play-chords (chords)
  (dolist (chord chords)
    (chord-play chord)
    (mysleep 1)))

(defun play-list-of-notes (list-of-notes)
  (dolist (note list-of-notes)
    (when note
      (schedule-note note))))

(defmacro c (fn &body body) `(,fn (list ,@(mapcar (lambda (x) `',x) body))))

(defun tonic-subdominant-dominant2 (scale)
  (->  (make-scale-chords scale)
;       (scale-chord-filter #'chord-filter (lambda (x) (chord-drop-root x scale)))
       (chord-seq '(I IV V I) 2)))

(defun tonic-subdominant-dominant (scale &optional (octave 4))
  (attach-type 'cadence-sequence
	       (list
		(solfanotes  scale '(DO MI SO) octave)
		(solfanotes  scale '(FA LA DO) octave)
		(solfanotes  scale '(SO TI RE) octave)
		(solfanotes  scale '(DO MI SO) octave))))

(defun play-tonic-subdominant-dominant (scale &optional (octave 4))
  (mapcar #'play-chord-notes-nonasync (tonic-subdominant-dominant (make-scale 'c4) octave)))

(defun play-tonic (scale) (note-play (car scale)))
(defun play-subdominant (scale) (note-play (nth 3 scale)))
(defun play-dominant (scale) (note-play (nth 4 scale)))
