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
    (re . (do mi))
    (mi . (so fa re do))
    (fa . (so la mi))
    (so . (fa ti do re))
    (la . (ti so))
    (ti . (la do))))

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
  (let ((note (random-element (scale-notes scale))))
    (make-cadence-sequence scale note)))

(defun make-random-melody-sequence (scale octave &optional (octave-range 1) (length 10))
  (make-cadence-sequence
   scale
   (mapcar (lambda (x) (find-solfege
			x
			(scale-octave-range octave (+ octave octave-range)
					    (scale-notes scale))))
	   (resolving-melody 'do (melody-rules) length))))

(defun make-rhythmic-melody-sequence (scale measure-count octave)
  (let* ((rhythm (flatten (append (make-measures measure-count) '(1))))
	 (melody-sequence (make-random-melody-sequence
			   (make-scale 'd4) 3 3 (- (length rhythm) 1))))
    (make-cadence-sequence scale
			   (make-rhythmic-notes (cadseq-select-melody melody-sequence) rhythm))))

(defun make-random-progression-sequence (scale &optional (chord-type-fn #'triads))
  (let ((random-sequence
	   (-> (make-scale-chords scale)
	       (scale-chord-filter #'chord-type-filter chord-type-fn)
	       (scale-chord-filter #'chord-filter #'chord-butfifth)
	       (scale-chord-filter #'chord-filter (lambda (x) (chord-drop-root x scale)))
	       (chord-seq (resolving-chord-progression 'I (chord-rules) ) 2))))
    (make-cadence-sequence scale random-sequence)))

;; Cadence Sequence
(defun make-cadence-sequence (scale item)
  (cons (tonic-subdominant-dominant2 scale) item))
(defun cadseq-select-cadence (cadence-sequence) (car cadence-sequence))
(defun select-progression (cadence-sequence) (cdr cadence-sequence))
(defun cadseq-select-melody (cadence-sequence) (cdr cadence-sequence))
(defun cadseq-select-note (cadence-sequence) (cdr cadence-sequence))

(defun play-cadence-progression-sequence (cadence-sequence)
  (chord-sequence-play (cadseq-select-cadence cadence-sequence))
  (sleep 1)
  (chord-sequence-play (select-progression cadence-sequence)))

(defun play-cadence-note-sequence (cadence-sequence)
  (chord-sequence-play (cadseq-select-cadence cadence-sequence))
  (sleep 0.5)
  (note-play (cadseq-select-note cadence-sequence)))

(defun play-cadence-melody-sequence (cadence-sequence)
  (chord-sequence-play (cadseq-select-cadence cadence-sequence))
  (sleep 0.5)
  (dolist (note (cadseq-select-melody cadence-sequence))
    (note-play note)
    (sleep 1)))

(defun play-rhythmic-melody-sequence (cadence-sequence)
  (chord-sequence-play (cadseq-select-cadence cadence-sequence))
  (sleep 0.5)
  (play-events
   (rhythmic-notes->pm-events (cadseq-select-melody cadence-sequence) 90)))

;; (play-cadence-melody-sequence (make-random-melody-sequence (make-scale 'c4) 4))
;; (play-cadence-progression-sequence (make-random-progression-sequence (make-scale 'c4)))
;; (play-cadence-note-sequence (make-random-note-sequence (make-scale 'c4)))
;; (play-rhythmic-melody-sequence (make-rhythmic-melody-sequence (make-scale 'c4) 2 2))


