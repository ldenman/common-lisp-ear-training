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
  (if (taggedp item)
      (cond
	((cadence-sequence? item) (play-chord-notes (untag item)))
	((chord-sequence? item) (chord-sequence-play (untag item))))
      (cond
	((chord? item) (chord-play-nonasync item))
	((note? item) (note-play item))
	((listp item) (play-chord-notes-nonasync item)))))

(defun play-random (scale) (note-play (car (random-note scale))))

;; assumes chord is list of chord tones
(defun chord-play-nonasync (chord &optional (sleep 0.5))
  (play-chord-notes-nonasync (chord-notes chord)))

;; assumes chord is list of chord tones
(defun chord-sequence-play (chord-sequence &optional (sleep 1))
  (dolist (chord (chord-sequence-chords chord-sequence))
    (chord-play-nonasync chord)
    (mysleep sleep)))

;; assumes chord-list is a list of list of notes
(defun play-chord-notes (chord-list &optional (sleep 1))
  (dolist (chord chord-list)
    (play-chord-notes-nonasync chord)
    (mysleep sleep)))

;; assumes chord is list of chord tones
(defun chord-play (chord &optional (sleep 1))
  (chord-play-nonasync chord sleep))
;(smoke-test)

(defun play-chord-notes-nonasync (notes &optional (sleep 1))
  (dolist (note notes)
    (when note
      (note-play note))))

;; assumes chord is list of chord tones
(defun play-chords (chords)
  (dolist (chord chords)
    (chord-play chord)
    (mysleep 1)))

(defun play-list-of-notes (list-of-notes)
  (dolist (note list-of-notes)
    (when note
      (schedule-note note))))

(defun tonic-subdominant-dominant (scale &optional (octave 4))
  (list
   (solfanotes  scale '(DO MI SO) octave)
   (solfanotes  scale '(FA LA DO) octave)
   (solfanotes  scale '(SO TI RE) octave)
   (solfanotes  scale '(DO MI SO) octave)))

(defun play-tonic-subdominant-dominant (scale &optional (octave 4))
  (play (tonic-subdominant-dominant scale octave)))

(defun play-tonic (scale) (play (car scale)))
(defun play-subdominant (scale) (play (nth 3 scale)))
(defun play-dominant (scale) (play (nth 4 scale)))
