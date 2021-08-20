(in-package :ld-music)

(defun play-random (scale) (note-play (car (random-note scale))))

(defun chord-play (chord &optional (sleep 1))
  (dolist (note (chord-notes chord))
    (if note
    (note-play note)))
  (sleep sleep))

(defun chord-seq-play (chord-seq)
  (chord-sequence-play (chord-sequence-chords chord-seq)))

(defun play-chords (chords)
  (dolist (chord chords)
    (chord-play chord)
    (sleep 1)))

(defun arp (l scale)
  (dolist
      (x (mapcar (lambda (x) (find-solfege x scale)) l))
    (note-play x) (sleep 0.5)))

(defun rarp (l scale)
  (let ((m (reverse (mapcar (lambda (x) (find-solfege x scale)) l))))
    (dolist (x m)
      (note-play x) (sleep 0.5))))

(defmacro c (fn &body body) `(,fn (list ,@(mapcar (lambda (x) `',x) body))))

(defun play-tonic (scale) (note-play (car scale)))
(defun play-subdominant (scale) (note-play (nth 3 scale)))
(defun play-dominant (scale) (note-play (nth 4 scale)))

(defun play-tonic-subdominant-dominant (scale)
  (progn
    (note-play (note-octave-down (car scale) scale))
    (solfege-chord '(DO MI SO) scale)
    (solfege-chord '(FA LA DO) scale)
    (solfege-chord '(SO TI RE) scale)
    (play-tonic scale)))

(defun smoke-test ()
  (note-play (make-note 'C4 72 nil)))

