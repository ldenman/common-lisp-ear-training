(in-package :ld-music)

;; Chord progressions
(defun chord-rules () '((I    . (II- III- IV V VI- VII))
			(V    . (I IV IV))
			(IV   . (I V VI- III-))
			(VI-  . (IV V I II-))
			(III- . (VI- I IV II-))
			(VII  . (I))
			(II-  . (V IV III-))))

(defun next-chord (chord chord-rules)
  (let ((next-chords (cdr (find-if (car-fn #'car-eq chord) chord-rules))))
    (if next-chords
	(random-element next-chords))))

(defun random-chord-progression (start-chord chord-rules n)
   (if (> n 0)
       (append
	(list start-chord)
	(random-chord-progression (next-chord start-chord chord-rules) chord-rules (- n 1)))))

(defun resolving-chord-progression (start-chord chord-rules &optional (length 4))
  (let ((chord-progression  (random-chord-progression start-chord (chord-rules) length)))
    (if (eq start-chord (car (last chord-progression)))
	chord-progression
	(append chord-progression (list start-chord)))))

;; Melody
(defun melody-rules ()
  '((do . (re ti mi so fa la))
    (re . (do mi fa))
    (mi . (so fa re do))
    (fa . (so la mi))
    (so . (fa ti do re))
    (la . (ti so do))
    (ti . (la re do))))

(defun next-note (note melody-rules)
  (random-element (cdr (find-if (car-fn #'car-eq note) melody-rules))))

(defun random-melody (start-note melody-rules n)
  (if (> n 0)
      (append
       (list start-note)
       (random-melody (next-note start-note melody-rules) melody-rules (- n 1)))))

(defun resolving-melody (end-note melody-rules n)
  (let ((melody (random-melody 'do melody-rules (- n 1))))
    (append melody (list end-note))))

;; Sequences
(defun make-random-note-sequence (scale)
  (let ((note (random-element (_notes scale))))
    (make-cadence-sequence scale note)))

(defun make-random-melody-sequence (scale octave &optional (octave-range 1) (length 10))
  (make-cadence-sequence
   scale
   (mapcar (lambda (x) (find-solfege2
			x
			(scale-octave-range octave (+ octave octave-range)
					    (_notes scale))))
	   (resolving-melody 'do (melody-rules) length))))

(defun make-rhythmic-melody-sequence (scale measure-count octave)
  (let* ((rhythm (flatten (append (make-measures measure-count) '(1))))
	 (melody-sequence (make-random-melody-sequence scale octave 3 (- (length rhythm) 1))))
    (make-cadence-sequence scale
			   (make-rhythmic-notes (cadseq-contents melody-sequence) rhythm))))

(defun make-random-progression-sequence (scale &optional (chord-type-fn #'triads))
  (let ((random-sequence
	   (-> (make-scale-chords scale chord-type-fn)
	       (chord-seq (resolving-chord-progression 'I (chord-rules) ) 4))))
    (make-cadence-sequence scale random-sequence)))

;; Cadence Sequence
(defun make-cadence-sequence (scale item)
  (cons (tonic-subdominant-dominant scale) item))

(defun cadseq-cadence (cadence-sequence) (car cadence-sequence))
(defun cadseq-contents (cadence-sequence) (cdr cadence-sequence))

(defun play-cadence-note-sequence (cadence-sequence)
  (play (cadseq-cadence cadence-sequence))
  (mysleep 0.5)
  (play (cadseq-contents cadence-sequence)))

(defun play-cadence-melody-sequence (cadence-sequence)
  (play (cadseq-cadence cadence-sequence))
  (mysleep 0.5)
  (dolist (note (cadseq-contents cadence-sequence))
    (note-play note)
    (sleep 1)))

(defun play-rhythmic-melody-sequence (cadence-sequence)
  (play (cadseq-cadence cadence-sequence))
  (mysleep 0.5)
  (play-events
   (rhythmic-notes->pm-events (cadseq-contents cadence-sequence) 90)))

(defun play-cadence-progression-sequence (cadence-sequence)
  (play (cadseq-cadence cadence-sequence))
  (mysleep 1)
  (play (cadseq-contents cadence-sequence)))
